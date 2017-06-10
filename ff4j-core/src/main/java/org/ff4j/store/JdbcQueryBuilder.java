package org.ff4j.store;

import static org.ff4j.audit.EventConstants.ACTION_CHECK_OK;
import static org.ff4j.audit.EventConstants.ACTION_CLEAR;
import static org.ff4j.audit.EventConstants.ACTION_CONNECT;
import static org.ff4j.audit.EventConstants.ACTION_CREATE;
import static org.ff4j.audit.EventConstants.ACTION_DELETE;
import static org.ff4j.audit.EventConstants.ACTION_DISCONNECT;
import static org.ff4j.audit.EventConstants.ACTION_TOGGLE_OFF;
import static org.ff4j.audit.EventConstants.ACTION_TOGGLE_ON;
import static org.ff4j.audit.EventConstants.ACTION_UPDATE;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_ACTION;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_HOSTNAME;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_NAME;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_SOURCE;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_TIME;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_TYPE;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_UUID;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_USER;

import java.util.Collection;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2016 FF4J
 * %%
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */


import org.ff4j.audit.EventConstants;
import org.ff4j.audit.EventQueryDefinition;

/**
 * Create JDBC queries for FF4J with capabilities to 
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class JdbcQueryBuilder {
	
    /** table prefix. */
	public String tablePrefix = "FF4J_";
	
	/** table suffix. */
	public String tableSuffix = "";

	/** 
	 * Default constructor. 
	 **/
	public JdbcQueryBuilder() {
	}
	
	/**
	 * Overriding Builder.
	 *
	 * @param prefix
	 * 		table prefix
	 * @param suffix
	 * 		table suffix
	 */
	public JdbcQueryBuilder(String prefix, String suffix) {
		this.tablePrefix = prefix;
		this.tableSuffix = suffix;
	}
	
	/**
	 * Prefix and suffix table Names.
	 * 
	 * @param coreName
	 *         current name
	 * @return
	 *         new table name
	 */
	public String getTableName(String coreName) {
		return tablePrefix + coreName + tableSuffix;
	}
	
	/**
	 * Table name for audit.
	 *
	 * @return
	 *     Table name for audit
	 */
	public String getTableNameAudit() {
        return getTableName("AUDIT");
    }
	
	/**
     * Table name for features.
     *
     * @return
     *     Table name for features
     */
	public String getTableNameFeatures() {
        return getTableName("FEATURES");
    }
	
	/**
     * Table name for roles.
     *
     * @return
     *     Table name for roles
     */
	public String getTableNameRoles() {
        return getTableName("ROLES");
    }

	/**
     * Table name for custom properties.
     *
     * @return
     *     Table name for custom properties.
     */
    public String getTableNameCustomProperties() {
        return getTableName("CUSTOM_PROPERTIES");
    }
    
    /**
     * Table name for properties.
     *
     * @return
     *     Table name for properties.
     */
    public String getTableNameProperties() {
        return getTableName("PROPERTIES");
    }
    
    /**
     * SQL to create Tables (won't work for all DB).
     *
     * @return
     *      sql to create features table
     */
    public String sqlCreateTableFeatures() {
        StringBuilder sb = new StringBuilder("CREATE TABLE ");
        sb.append(getTableNameFeatures());
        sb.append("( FEAT_UID    VARCHAR(100), "
                  + "ENABLE      INTEGER NOT NULL, "
                  + "DESCRIPTION VARCHAR(1000), "
                  + "STRATEGY    VARCHAR(1000), "
                  + "EXPRESSION  VARCHAR(255), "
                  + "GROUPNAME   VARCHAR(100), "
                  + "PRIMARY KEY(FEAT_UID))");
        return sb.toString();
    }
    
    /**
     * SQL to create Tables (won't work for all DB).
     *
     * @return
     *      sql to create roles table
     */
    public String sqlCreateTableRoles() {
        StringBuilder sb = new StringBuilder("CREATE TABLE ");
        sb.append(getTableNameRoles());
        sb.append("( FEAT_UID VARCHAR(100) REFERENCES " 
                + getTableNameFeatures() + "(FEAT_UID), "
                + "ROLE_NAME  VARCHAR(100), "
                + "PRIMARY KEY(FEAT_UID, ROLE_NAME))");
        return sb.toString();
    }
    
    /**
     * SQL to create Tables (won't work for all DB).
     *
     * @return
     *      sql to create customproperties table
     */
    public String sqlCreateTableCustomProperties() {
        StringBuilder sb = new StringBuilder("CREATE TABLE ");
        sb.append(getTableNameCustomProperties());
        sb.append("( PROPERTY_ID   VARCHAR(100) NOT NULL,"
                 + " CLAZZ          VARCHAR(255) NOT NULL,"
                 + " CURRENTVALUE  VARCHAR(255),"
                 + " FIXEDVALUES   VARCHAR(1000),"
                 + " DESCRIPTION   VARCHAR(1000),"
                 + " FEAT_UID      VARCHAR(100) REFERENCES " + getTableNameFeatures() + "(FEAT_UID),"
                 + " PRIMARY KEY(PROPERTY_ID, FEAT_UID))");
        return sb.toString();
    }
    
    /**
     * SQL to create Tables (won't work for all JDBC implementations).
     *
     * @return
     *      sql to create audit properties
     */
    public String sqlCreateTableProperties() {
        StringBuilder sb = new StringBuilder("CREATE TABLE ");
        sb.append(getTableNameProperties());
        sb.append("( PROPERTY_ID  VARCHAR(100) NOT NULL,"
                 + " CLAZZ        VARCHAR(255) NOT NULL,"
                 + " CURRENTVALUE VARCHAR(255),"
                 + " FIXEDVALUES  VARCHAR(1000),"
                 + " DESCRIPTION  VARCHAR(1000),"
                 + " PRIMARY KEY(PROPERTY_ID))");
        return sb.toString();
    }
    
    /**
     * SQL to create Tables (won't work for all DB).
     *
     * @return
     *      sql to create audit table
     */
    public String sqlCreateTableAudit() {
        StringBuilder sb = new StringBuilder("CREATE TABLE ");
        sb.append(getTableNameAudit());
        sb.append("( EVT_UUID    VARCHAR(40)  NOT NULL,"
                + " EVT_TIME     TIMESTAMP    NOT NULL,"
                + " EVT_TYPE     VARCHAR(30)  NOT NULL,"
                + " EVT_NAME     VARCHAR(30)  NOT NULL,"
                + " EVT_ACTION   VARCHAR(30)  NOT NULL,"
                + " EVT_HOSTNAME VARCHAR(100) NOT NULL,"
                + " EVT_SOURCE   VARCHAR(30)  NOT NULL,"
                + " EVT_DURATION INTEGER,"
                + " EVT_USER     VARCHAR(30),"
                + " EVT_VALUE    VARCHAR(100),"
                + " EVT_KEYS     VARCHAR(255),"
                + "PRIMARY KEY(EVT_UUID, EVT_TIME))");
         return sb.toString();
    }
    
    
	public String getAllFeatures() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT FEAT_UID,ENABLE,DESCRIPTION,STRATEGY,EXPRESSION,GROUPNAME FROM ");
		sb.append(getTableNameFeatures());
		return sb.toString();
	}
	
	public String getAllGroups() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT DISTINCT(GROUPNAME) FROM ");
		sb.append(getTableNameFeatures());
		return sb.toString();
	}
	
	public String getFeatureOfGroup() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT FEAT_UID,ENABLE,DESCRIPTION,STRATEGY,EXPRESSION,GROUPNAME FROM ");
		sb.append(getTableNameFeatures());
		sb.append(" WHERE GROUPNAME = ?");
		return sb.toString();
	}
	
	public String getFeature() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT FEAT_UID,ENABLE,DESCRIPTION,STRATEGY,EXPRESSION,GROUPNAME FROM ");
		sb.append(getTableNameFeatures());
		sb.append(" WHERE FEAT_UID = ?");
		return sb.toString();
	}
	
	public String existFeature() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT COUNT(FEAT_UID) FROM ");
		sb.append(getTableNameFeatures());
		sb.append(" WHERE FEAT_UID = ?");
		return sb.toString();
	}
	
	public String existGroup() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT COUNT(FEAT_UID) FROM ");
		sb.append(getTableNameFeatures());
		sb.append(" WHERE GROUPNAME = ?");
		return sb.toString();
	}
	
	public String enableFeature() {
		StringBuilder sb = new StringBuilder();
		sb.append("UPDATE ");
		sb.append(getTableNameFeatures());
		sb.append(" SET ENABLE = 1 WHERE FEAT_UID = ?");
		return sb.toString();
	}
	
	public String enableGroup() {
		StringBuilder sb = new StringBuilder();
		sb.append("UPDATE ");
		sb.append(getTableNameFeatures());
		sb.append(" SET ENABLE = 1 WHERE GROUPNAME = ?");
		return sb.toString();
	}
	
	public String disableFeature() {
		StringBuilder sb = new StringBuilder();
		sb.append("UPDATE ");
		sb.append(getTableNameFeatures());
		sb.append(" SET ENABLE = 0 WHERE FEAT_UID = ?");
		return sb.toString();
	}
	
	public String disableGroup() {
		StringBuilder sb = new StringBuilder();
		sb.append("UPDATE ");
		sb.append(getTableNameFeatures());
		sb.append(" SET ENABLE = 0 WHERE GROUPNAME = ?");
		return sb.toString();
	}
	
	public String addFeatureToGroup() {
		StringBuilder sb = new StringBuilder();
		sb.append("UPDATE ");
		sb.append(getTableNameFeatures());
		sb.append(" SET GROUPNAME = ? WHERE FEAT_UID = ?");
		return sb.toString();
	}
	
	public String removeFeatureFromGroup() {
		StringBuilder sb = new StringBuilder();
		sb.append("UPDATE ");
		sb.append(getTableNameFeatures());
		sb.append(" SET GROUPNAME = NULL WHERE FEAT_UID = ?");
		return sb.toString();
	}
	
	public String createFeature() {
		StringBuilder sb = new StringBuilder();
		sb.append("INSERT INTO ");
		sb.append(getTableNameFeatures());
		sb.append("(FEAT_UID, ENABLE, DESCRIPTION, STRATEGY,EXPRESSION, GROUPNAME) VALUES(?, ?, ?, ?, ?, ?)");
		return sb.toString();
	}
	
	public String deleteFeature() {
		StringBuilder sb = new StringBuilder();
		sb.append("DELETE FROM ");
		sb.append(getTableNameFeatures());
		sb.append(" WHERE FEAT_UID = ?");
		return sb.toString();
	}

	public String updateFeature() {
		StringBuilder sb = new StringBuilder();
		sb.append("UPDATE ");
		sb.append(getTableNameFeatures());
		sb.append(" SET ENABLE=?,DESCRIPTION=?,STRATEGY=?,EXPRESSION=?,GROUPNAME=? WHERE FEAT_UID = ?");
		return sb.toString();
	}
	
	public String addRoleToFeature() {
		StringBuilder sb = new StringBuilder();
		sb.append("INSERT INTO ");
		sb.append(getTableNameRoles());
		sb.append(" (FEAT_UID, ROLE_NAME) VALUES (?,?)");
		return sb.toString();
	}
	
	public String deleteRoles() {
		StringBuilder sb = new StringBuilder();
		sb.append("DELETE FROM ");
		sb.append(getTableNameRoles());
        sb.append(" WHERE FEAT_UID = ?");
		return sb.toString();
	}
	
	public String deleteFeatureRole() {
		StringBuilder sb = new StringBuilder();
		sb.append("DELETE FROM ");
		sb.append(getTableNameRoles());
        sb.append(" WHERE FEAT_UID = ? AND ROLE_NAME = ?");
		return sb.toString();
	}
	
	public String getRoles() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT ROLE_NAME FROM ");
		sb.append(getTableNameRoles());
		sb.append(" WHERE FEAT_UID = ?");
		return sb.toString();
	}
	
	public String getAllRoles() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT FEAT_UID,ROLE_NAME FROM ");
		sb.append(getTableNameRoles());
		return sb.toString(); 
	}
	
    // ------- Properties -------------
    
	public String getFeatureProperties() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT PROPERTY_ID,CLAZZ,CURRENTVALUE,DESCRIPTION,FIXEDVALUES,FEAT_UID FROM ");
		sb.append(getTableNameCustomProperties());
		sb.append(" WHERE FEAT_UID = ?");
		return sb.toString();
	}
	
	public String getFeatureProperty() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT PROPERTY_ID,CLAZZ,CURRENTVALUE,FIXEDVALUES,FEAT_UID FROM ");
		sb.append(getTableNameCustomProperties());
		sb.append(" WHERE PROPERTY_ID = ? AND FEAT_UID = ?");
		return sb.toString();
	}
	
	public String deleteFeatureProperty() {
		StringBuilder sb = new StringBuilder();
		sb.append("DELETE FROM ");
		sb.append(getTableNameCustomProperties());
		sb.append(" WHERE PROPERTY_ID = ? AND FEAT_UID = ?");
		return sb.toString();
	}
    
	public String deleteAllFeatureCustomProperties() {
		StringBuilder sb = new StringBuilder();
		sb.append("DELETE FROM ");
		sb.append(getTableNameCustomProperties());
		sb.append(" WHERE FEAT_UID = ?");
		return sb.toString();
	}
	
	public String deleteAllCustomProperties() {
		StringBuilder sb = new StringBuilder();
		sb.append("DELETE FROM ");
		sb.append(getTableNameCustomProperties());
		return sb.toString();
	}
	
	public String deleteAllRoles() {
		StringBuilder sb = new StringBuilder();
		sb.append("DELETE FROM ");
		sb.append(getTableNameRoles());
		return sb.toString();
	}
	
	public String deleteAllFeatures() {
		StringBuilder sb = new StringBuilder();
		sb.append("DELETE FROM ");
		sb.append(getTableNameFeatures());
		return sb.toString();
	}
	
	public String createFeatureProperty() {
		StringBuilder sb = new StringBuilder();
		sb.append("INSERT INTO ");
		sb.append(getTableNameCustomProperties());
		sb.append("(PROPERTY_ID, CLAZZ, CURRENTVALUE, DESCRIPTION, FIXEDVALUES, FEAT_UID) VALUES(?, ?, ?, ?, ?, ?)");
		return sb.toString();
	}
	
	public String createProperty() {
		StringBuilder sb = new StringBuilder();
		sb.append("INSERT INTO ");
		sb.append(getTableNameProperties());
		sb.append("(PROPERTY_ID, CLAZZ, CURRENTVALUE, DESCRIPTION, FIXEDVALUES) VALUES(?, ?, ?, ?, ?)");
		return sb.toString();
	}

	public String deleteProperty() {
		StringBuilder sb = new StringBuilder();
		sb.append("DELETE FROM ");
		sb.append(getTableNameProperties());
		sb.append(" WHERE PROPERTY_ID = ?");
		return sb.toString();
	}
	
	public String deleteAllProperties() {
		StringBuilder sb = new StringBuilder();
		sb.append("DELETE FROM ");
		sb.append(getTableName("PROPERTIES"));
		return sb.toString();
	}
	
	public String existProperty() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT COUNT(*) FROM ");
		sb.append(getTableNameProperties());
		sb.append(" WHERE PROPERTY_ID = ?");
		return sb.toString();
	}
	
	public String getProperty() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT PROPERTY_ID,CLAZZ,CURRENTVALUE,DESCRIPTION,FIXEDVALUES FROM ");
		sb.append(getTableNameProperties());
		sb.append(" WHERE PROPERTY_ID = ?");
		return sb.toString();
	}
	
	public String updateProperty() {
		StringBuilder sb = new StringBuilder();
		sb.append("UPDATE ");
		sb.append(getTableNameProperties());
		sb.append(" SET CURRENTVALUE = ? WHERE PROPERTY_ID = ?");
		return sb.toString();
	}
	
	public String getAllProperties() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT PROPERTY_ID,CLAZZ,CURRENTVALUE,DESCRIPTION,FIXEDVALUES FROM ");
		sb.append(getTableNameProperties());
		return sb.toString();
	}
	
	public String getAllPropertiesNames() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT PROPERTY_ID FROM ");
		sb.append(getTableNameProperties());
		return sb.toString();
	}
	
    // ------- AUDIT -------------
	
	public String getEventByUuidQuery() {
	     StringBuilder sb = new StringBuilder();
	     sb.append("SELECT * FROM ");
	     sb.append(getTableNameAudit());
	     sb.append(" WHERE " + COL_EVENT_UUID + " LIKE ?");
	     return sb.toString();
	}
	
	public String getPurgeFeatureUsageQuery(EventQueryDefinition eqd) {
	    StringBuilder sb = new StringBuilder();
        sb.append("DELETE FROM ");
        sb.append(getTableNameAudit());
        sb.append(buildWhereClause(eqd, true, false));
        return sb.toString();
	}
	
	public String getSelectFeatureUsageQuery(EventQueryDefinition eqd) {
        StringBuilder sb = new StringBuilder();
        sb.append("SELECT * FROM ");
        sb.append(getTableNameAudit());
        sb.append(buildWhereClause(eqd, true, false));
        return sb.toString();
    }
	
    public String getPurgeAuditTrailQuery(EventQueryDefinition eqd) {
        StringBuilder sb = new StringBuilder();
        sb.append("DELETE FROM ");
        sb.append(getTableNameAudit());
        sb.append(buildWhereClause(eqd, false, true));
        return sb.toString();
    }
	
	public String getSelectAuditTrailQuery(EventQueryDefinition eqd) {
        StringBuilder sb = new StringBuilder();
        sb.append("SELECT * FROM ");
        sb.append(getTableNameAudit());
        sb.append(buildWhereClause(eqd, false, true));
        return sb.toString();
    }
	
	public String getHitCount(String columName) {
	    StringBuilder sb = new StringBuilder();
        sb.append("SELECT count(" + COL_EVENT_UUID + ") as NB, " + columName + " FROM ");
        sb.append(getTableNameAudit());
        sb.append(" WHERE (" + COL_EVENT_TYPE   + " LIKE '" + EventConstants.TARGET_FEATURE  + "') ");
        sb.append(" AND   (" + COL_EVENT_ACTION + " LIKE '" + EventConstants.ACTION_CHECK_OK + "') ");
        sb.append(" AND   (" + COL_EVENT_TIME + "> ?) ");
        sb.append(" AND   (" + COL_EVENT_TIME + "< ?)");
        sb.append(" GROUP BY " + columName);
        return sb.toString();
	}
	
	public String getFeaturesHitCount() {
	    return getHitCount(COL_EVENT_NAME);
    }
	
	public String getHostHitCount() {
	    return getHitCount(COL_EVENT_HOSTNAME);
    }
	
	public String getUserHitCount() {
	    return getHitCount(COL_EVENT_USER);
    }
	
	public String getSourceHitCount() {
        return getHitCount(COL_EVENT_SOURCE);
    }
	
	// -------
   
    public String getFeatureDistributionAudit() {
        StringBuilder sb = new StringBuilder();
        sb.append("SELECT count(" + COL_EVENT_UUID + ") as NB, " + COL_EVENT_ACTION + " FROM ");
        sb.append(getTableNameAudit());
        sb.append(" WHERE (" + COL_EVENT_TYPE + " LIKE '" + EventConstants.TARGET_FEATURE  + "') ");
        sb.append(" AND   (" + COL_EVENT_NAME + " LIKE ?) ");
        sb.append(" AND   (" + COL_EVENT_TIME + "> ?) ");
        sb.append(" AND   (" + COL_EVENT_TIME + "< ?)");
        sb.append(" GROUP BY " + COL_EVENT_ACTION);
        return sb.toString();
    }
   
    
	private String buildClauseIn(Collection < String> elements) {
	    boolean first = true;
	    StringBuilder sb = new StringBuilder("(");
	    for (String el : elements) {
	        if (!first) {
	            sb.append(",");
	        }
	        sb.append("'");
	        sb.append(el);
	        sb.append("'");
            first = false;
        }
	    sb.append(")");
	    return sb.toString();
	}
	
    public String buildWhereClause(EventQueryDefinition qDef, boolean filterForCheck, boolean filterAuditTrail) {
        StringBuilder sb = new StringBuilder();
        sb.append(" WHERE (" + COL_EVENT_TIME + "> ?) ");
        sb.append(" AND   (" + COL_EVENT_TIME + "< ?) ");
        // If a dedicated filter is there use it
        if (qDef.getActionFilters().isEmpty()) {
            if (filterForCheck) {
                qDef.getActionFilters().add(ACTION_CHECK_OK);
            }
            if (filterAuditTrail) {
                qDef.getActionFilters().add(ACTION_CONNECT);
                qDef.getActionFilters().add(ACTION_DISCONNECT);
                qDef.getActionFilters().add(ACTION_TOGGLE_ON);
                qDef.getActionFilters().add(ACTION_TOGGLE_OFF);
                qDef.getActionFilters().add(ACTION_CREATE);
                qDef.getActionFilters().add(ACTION_DELETE);
                qDef.getActionFilters().add(ACTION_UPDATE);
                qDef.getActionFilters().add(ACTION_CLEAR);
            }
        }
        if (qDef.getActionFilters() != null && !qDef.getActionFilters().isEmpty()) {
            sb.append(" AND (" + COL_EVENT_ACTION + " IN ");
            sb.append(buildClauseIn(qDef.getActionFilters()));
            sb.append(")");
        }
        if (qDef.getHostFilters() != null && !qDef.getHostFilters().isEmpty()) {
            sb.append(" AND (" + COL_EVENT_HOSTNAME + " IN ");
            sb.append(buildClauseIn(qDef.getHostFilters()));
            sb.append(")");
            
        }
        if (qDef.getNamesFilter() != null && !qDef.getNamesFilter().isEmpty()) {
            sb.append(" AND (" + COL_EVENT_NAME + " IN ");
            sb.append(buildClauseIn(qDef.getNamesFilter()));
            sb.append(")");
        }
        if (qDef.getSourceFilters() != null && !qDef.getSourceFilters().isEmpty()) {
            sb.append(" AND (" + COL_EVENT_SOURCE + " IN ");
            sb.append(buildClauseIn(qDef.getSourceFilters()));
            sb.append(")");
        }
        return sb.toString();
    }
    
    // ---------- Queries for AUDIT ----------------------
    
    public String sqlSaveAudit() {
        StringBuilder sb = new StringBuilder();
        sb.append("INSERT INTO ");
        sb.append(getTableNameAudit());
        sb.append("(EVT_UUID, EVT_TIME, EVT_TYPE, EVT_NAME, EVT_ACTION,"
                 + "EVT_HOSTNAME, EVT_SOURCE, EVT_DURATION, " 
                 + "EVT_USER, EVT_VALUE, EVT_KEYS) "
                 + "VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)");
        return sb.toString();
    }
    
	
}
