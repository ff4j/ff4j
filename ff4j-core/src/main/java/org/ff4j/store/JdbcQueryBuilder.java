package org.ff4j.store;

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

/**
 * Create JDBC queries for FF4J with capabilities to 
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class JdbcQueryBuilder {
	
	/** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_UID = "FEAT_UID";

    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_ENABLE = "ENABLE";

    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_DESCRIPTION = "DESCRIPTION";

    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_GROUPNAME = "GROUPNAME";

    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_STRATEGY = "STRATEGY";

    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_EXPRESSION = "EXPRESSION";

    /** sql column name from table FF4J_ROLES. */
    public static final String COL_ROLE_FEATID = "FEAT_UID";

    /** sql column name from table FF4J_ROLES. */
    public static final String COL_ROLE_ROLENAME = "ROLE_NAME";
    
    /** sql column name from table FF4J_PROPERTIES. */
    public static final String COL_PROPERTY_ID = "PROPERTY_ID";
    
    /** sql column name from table FF4J_PROPERTIES. */
    public static final String COL_PROPERTY_TYPE = "CLAZZ";
    
    /** sql column name from table FF4J_PROPERTIES. */
    public static final String COL_PROPERTY_VALUE = "CURRENTVALUE";
    
    /** sql column name from table FF4J_PROPERTIES. */
    public static final String COL_PROPERTY_FIXED = "FIXEDVALUES";
    
    /** sql column name from table FF4J_PROPERTIES. */
    public static final String COL_PROPERTY_FEATID = "FEAT_UID";
    
    /** sql column name from table FF4J_PROPERTIES. */
    public static final String COL_PROPERTY_DESCRIPTION = "DESCRIPTION";
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_UUID = "EVT_UUID";
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_TIME = "EVT_TIME";
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_TYPE = "EVT_TYPE";
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_NAME = "EVT_NAME";
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_ACTION = "EVT_ACTION";
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_HOSTNAME = "EVT_HOSTNAME";
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_SOURCE = "EVT_SOURCE";
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_DURATION = "EVT_DURATION";
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_USER = "EVT_USER";
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_VALUE = "EVT_VALUE";
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_KEYS = "EVT_KEYS";
    
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
	
	public String getTableName(String coreName) {
		return tablePrefix + coreName + tableSuffix;
	}
	
	public String getAllFeatures() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT FEAT_UID,ENABLE,DESCRIPTION,STRATEGY,EXPRESSION,GROUPNAME FROM ");
		sb.append(getTableName("FEATURES"));
		return sb.toString();
	}
	
	public String getAllGroups() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT DISTINCT(GROUPNAME) FROM ");
		sb.append(getTableName("FEATURES"));
		return sb.toString();
	}
	
	public String getFeatureOfGroup() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT FEAT_UID,ENABLE,DESCRIPTION,STRATEGY,EXPRESSION,GROUPNAME FROM ");
		sb.append(getTableName("FEATURES"));
		sb.append(" WHERE GROUPNAME = ?");
		return sb.toString();
	}
	
	public String getFeature() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT FEAT_UID,ENABLE,DESCRIPTION,STRATEGY,EXPRESSION,GROUPNAME FROM ");
		sb.append(getTableName("FEATURES"));
		sb.append(" WHERE FEAT_UID = ?");
		return sb.toString();
	}
	
	public String existFeature() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT COUNT(FEAT_UID) FROM ");
		sb.append(getTableName("FEATURES"));
		sb.append(" WHERE FEAT_UID = ?");
		return sb.toString();
	}
	
	public String existGroup() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT COUNT(FEAT_UID) FROM ");
		sb.append(getTableName("FEATURES"));
		sb.append(" WHERE GROUPNAME = ?");
		return sb.toString();
	}
	
	public String enableFeature() {
		StringBuilder sb = new StringBuilder();
		sb.append("UPDATE ");
		sb.append(getTableName("FEATURES"));
		sb.append(" SET ENABLE = 1 WHERE FEAT_UID = ?");
		return sb.toString();
	}
	
	public String enableGroup() {
		StringBuilder sb = new StringBuilder();
		sb.append("UPDATE ");
		sb.append(getTableName("FEATURES"));
		sb.append(" SET ENABLE = 1 WHERE GROUPNAME = ?");
		return sb.toString();
	}
	
	public String disableFeature() {
		StringBuilder sb = new StringBuilder();
		sb.append("UPDATE ");
		sb.append(getTableName("FEATURES"));
		sb.append(" SET ENABLE = 0 WHERE FEAT_UID = ?");
		return sb.toString();
	}
	
	public String disableGroup() {
		StringBuilder sb = new StringBuilder();
		sb.append("UPDATE ");
		sb.append(getTableName("FEATURES"));
		sb.append(" SET ENABLE = 0 WHERE GROUPNAME = ?");
		return sb.toString();
	}
	
	public String addFeatureToGroup() {
		StringBuilder sb = new StringBuilder();
		sb.append("UPDATE ");
		sb.append(getTableName("FEATURES"));
		sb.append(" SET GROUPNAME = ? WHERE FEAT_UID = ?");
		return sb.toString();
	}
	
	public String removeFeatureFromGroup() {
		StringBuilder sb = new StringBuilder();
		sb.append("UPDATE ");
		sb.append(getTableName("FEATURES"));
		sb.append(" SET GROUPNAME = NULL WHERE FEAT_UID = ?");
		return sb.toString();
	}
	
	public String createFeature() {
		StringBuilder sb = new StringBuilder();
		sb.append("INSERT INTO ");
		sb.append(getTableName("FEATURES"));
		sb.append("(FEAT_UID, ENABLE, DESCRIPTION, STRATEGY,EXPRESSION, GROUPNAME) VALUES(?, ?, ?, ?, ?, ?)");
		return sb.toString();
	}
	
	public String deleteFeature() {
		StringBuilder sb = new StringBuilder();
		sb.append("DELETE FROM ");
		sb.append(getTableName("FEATURES"));
		sb.append(" WHERE FEAT_UID = ?");
		return sb.toString();
	}

	public String updateFeature() {
		StringBuilder sb = new StringBuilder();
		sb.append("UPDATE ");
		sb.append(getTableName("FEATURES"));
		sb.append(" SET ENABLE=?,DESCRIPTION=?,STRATEGY=?,EXPRESSION=?,GROUPNAME=? WHERE FEAT_UID = ?");
		return sb.toString();
	}
	
	public String addRoleToFeature() {
		StringBuilder sb = new StringBuilder();
		sb.append("INSERT INTO ");
		sb.append(getTableName("ROLES"));
		sb.append(" (FEAT_UID, ROLE_NAME) VALUES (?,?)");
		return sb.toString();
	}
	
	public String deleteRoles() {
		StringBuilder sb = new StringBuilder();
		sb.append("DELETE FROM ");
		sb.append(getTableName("ROLES"));
		sb.append(" WHERE FEAT_UID = ?");
		return sb.toString();
	}
	
	public String deleteFeatureRole() {
		StringBuilder sb = new StringBuilder();
		sb.append("DELETE FROM ");
		sb.append(getTableName("ROLES"));
		sb.append(" WHERE FEAT_UID = ? AND ROLE_NAME = ?");
		return sb.toString();
	}
	
	public String getRoles() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT ROLE_NAME FROM ");
		sb.append(getTableName("ROLES"));
		sb.append(" WHERE FEAT_UID = ?");
		return sb.toString();
	}
	
	public String getAllRoles() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT FEAT_UID,ROLE_NAME FROM ");
		sb.append(getTableName("ROLES"));
		return sb.toString(); 
	}
	
    // ------- Properties -------------
    
	public String getFeatureProperties() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT PROPERTY_ID,CLAZZ,CURRENTVALUE,DESCRIPTION,FIXEDVALUES,FEAT_UID FROM ");
		sb.append(getTableName("CUSTOM_PROPERTIES"));
		sb.append(" WHERE FEAT_UID = ?");
		return sb.toString();
	}
	
	public String getFeatureProperty() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT PROPERTY_ID,CLAZZ,CURRENTVALUE,FIXEDVALUES,FEAT_UID FROM ");
		sb.append(getTableName("CUSTOM_PROPERTIES"));
		sb.append(" WHERE PROPERTY_ID = ? AND FEAT_UID = ?");
		return sb.toString();
	}
	
	public String deleteFeatureProperty() {
		StringBuilder sb = new StringBuilder();
		sb.append("DELETE FROM ");
		sb.append(getTableName("CUSTOM_PROPERTIES"));
		sb.append(" WHERE PROPERTY_ID = ? AND FEAT_UID = ?");
		return sb.toString();
	}
    
	public String deleteAllFeatureCustomProperties() {
		StringBuilder sb = new StringBuilder();
		sb.append("DELETE FROM ");
		sb.append(getTableName("CUSTOM_PROPERTIES"));
		sb.append(" WHERE FEAT_UID = ?");
		return sb.toString();
	}
	
	public String deleteAllCustomProperties() {
		StringBuilder sb = new StringBuilder();
		sb.append("DELETE FROM ");
		sb.append(getTableName("CUSTOM_PROPERTIES"));
		return sb.toString();
	}
	
	public String deleteAllRoles() {
		StringBuilder sb = new StringBuilder();
		sb.append("DELETE FROM ");
		sb.append(getTableName("ROLES"));
		return sb.toString();
	}
	
	public String deleteAllFeatures() {
		StringBuilder sb = new StringBuilder();
		sb.append("DELETE FROM ");
		sb.append(getTableName("FEATURES"));
		return sb.toString();
	}
	
	public String createFeatureProperty() {
		StringBuilder sb = new StringBuilder();
		sb.append("INSERT INTO ");
		sb.append(getTableName("CUSTOM_PROPERTIES"));
		sb.append("(PROPERTY_ID, CLAZZ, CURRENTVALUE, DESCRIPTION, FIXEDVALUES, FEAT_UID) VALUES(?, ?, ?, ?, ?, ?)");
		return sb.toString();
	}
	
	public String createProperty() {
		StringBuilder sb = new StringBuilder();
		sb.append("INSERT INTO ");
		sb.append(getTableName("PROPERTIES"));
		sb.append("(PROPERTY_ID, CLAZZ, CURRENTVALUE, DESCRIPTION, FIXEDVALUES) VALUES(?, ?, ?, ?, ?)");
		return sb.toString();
	}

	public String deleteProperty() {
		StringBuilder sb = new StringBuilder();
		sb.append("DELETE FROM ");
		sb.append(getTableName("PROPERTIES"));
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
		sb.append(getTableName("PROPERTIES"));
		sb.append(" WHERE PROPERTY_ID = ?");
		return sb.toString();
	}
	
	public String getProperty() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT PROPERTY_ID,CLAZZ,CURRENTVALUE,DESCRIPTION,FIXEDVALUES FROM ");
		sb.append(getTableName("PROPERTIES"));
		sb.append(" WHERE PROPERTY_ID = ?");
		return sb.toString();
	}
	
	public String updateProperty() {
		StringBuilder sb = new StringBuilder();
		sb.append("UPDATE ");
		sb.append(getTableName("PROPERTIES"));
		sb.append(" SET CURRENTVALUE = ? WHERE PROPERTY_ID = ?");
		return sb.toString();
	}
	
	public String getAllProperties() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT PROPERTY_ID,CLAZZ,CURRENTVALUE,DESCRIPTION,FIXEDVALUES FROM ");
		sb.append(getTableName("PROPERTIES"));
		return sb.toString();
	}
	
	public String getAllPropertiesNames() {
		StringBuilder sb = new StringBuilder();
		sb.append("SELECT PROPERTY_ID FROM ");
		sb.append(getTableName("PROPERTIES"));
		return sb.toString();
	}
	
    // ------- AUDIT -------------
     
    public String countAudit() {
    	StringBuilder sb = new StringBuilder();
		sb.append("SELECT COUNT(*) FROM ");
		sb.append(getTableName("AUDIT"));
		return sb.toString();
    }
    
    public String listFeaturesAudit() {
    	StringBuilder sb = new StringBuilder();
		sb.append("SELECT DISTINCT " + COL_EVENT_NAME + " FROM ");
		sb.append(getTableName("AUDIT"));
		sb.append(" WHERE " + COL_EVENT_TYPE + " LIKE '" + EventConstants.TARGET_FEATURE + "'");
		return sb.toString();
    }
    
    public String getFeaturesPieAudit() {
    	StringBuilder sb = new StringBuilder();
		sb.append("SELECT count(" + COL_EVENT_UUID + ") as NB, " + COL_EVENT_NAME + " FROM ");
		sb.append(getTableName("AUDIT"));
		sb.append(" WHERE (" + COL_EVENT_TYPE   + " LIKE '" + EventConstants.TARGET_FEATURE  + "') ");
		sb.append(" AND   (" + COL_EVENT_ACTION + " LIKE '" + EventConstants.ACTION_CHECK_OK + "') ");
		sb.append(" AND   (" + COL_EVENT_TIME + "> ?) ");
		sb.append(" AND   (" + COL_EVENT_TIME + "< ?)");
		sb.append(" GROUP BY " + COL_EVENT_NAME);
		return sb.toString();
    }
   
    public String getFeatureDistributionAudit() {
    	StringBuilder sb = new StringBuilder();
		sb.append("SELECT count(" + COL_EVENT_UUID + ") as NB, " + COL_EVENT_ACTION + " FROM ");
		sb.append(getTableName("AUDIT"));
		sb.append(" WHERE (" + COL_EVENT_TYPE + " LIKE '" + EventConstants.TARGET_FEATURE  + "') ");
		sb.append(" AND   (" + COL_EVENT_NAME + " LIKE ?) ");
		sb.append(" AND   (" + COL_EVENT_TIME + "> ?) ");
		sb.append(" AND   (" + COL_EVENT_TIME + "< ?)");
        sb.append(" GROUP BY " + COL_EVENT_ACTION);
		return sb.toString();
    }
   
    public String getAllEventsFeatureAudit() {
    	StringBuilder sb = new StringBuilder();
		sb.append("SELECT * FROM ");
		sb.append(getTableName("AUDIT"));
		sb.append(" WHERE (" + COL_EVENT_TYPE + " LIKE '" + EventConstants.TARGET_FEATURE  + "') ");
		sb.append(" AND   (" + COL_EVENT_NAME + " LIKE ? )");
		sb.append(" AND   (" + COL_EVENT_TIME + "> ?)");
		sb.append(" AND   (" + COL_EVENT_TIME + "< ?)");
		return sb.toString();
    }
	
}
