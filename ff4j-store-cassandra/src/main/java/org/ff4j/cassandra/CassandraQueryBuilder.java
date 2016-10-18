package org.ff4j.cassandra;

import static org.ff4j.audit.EventConstants.ACTION_CHECK_OK;
import static org.ff4j.cassandra.CassandraConstants.COLUMN_FAMILY_AUDIT;

/*
 * #%L
 * ff4j-store-cassandra
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

import static org.ff4j.cassandra.CassandraConstants.COLUMN_FAMILY_FEATURES;
import static org.ff4j.cassandra.CassandraConstants.COLUMN_FAMILY_PROPERTIES;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_ACTION;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_DATE;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_DURATION;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_HOSTNAME;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_KEYS;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_NAME;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_SOURCE;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_TIME;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_TYPE;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_UID;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_USER;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_VALUE;
import static org.ff4j.cassandra.CassandraConstants.COL_FEAT_CUSTOMPROPERTIES;
import static org.ff4j.cassandra.CassandraConstants.COL_FEAT_DESCRIPTION;
import static org.ff4j.cassandra.CassandraConstants.COL_FEAT_ENABLE;
import static org.ff4j.cassandra.CassandraConstants.COL_FEAT_GROUPNAME;
import static org.ff4j.cassandra.CassandraConstants.COL_FEAT_ROLES;
import static org.ff4j.cassandra.CassandraConstants.COL_FEAT_STRATEGY;
import static org.ff4j.cassandra.CassandraConstants.COL_FEAT_UID;
import static org.ff4j.cassandra.CassandraConstants.COL_PROPERTY_CLAZZ;
import static org.ff4j.cassandra.CassandraConstants.COL_PROPERTY_DESCRIPTION;
import static org.ff4j.cassandra.CassandraConstants.COL_PROPERTY_FIXED;
import static org.ff4j.cassandra.CassandraConstants.COL_PROPERTY_ID;
import static org.ff4j.cassandra.CassandraConstants.COL_PROPERTY_VALUE;

import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;

import org.ff4j.audit.EventConstants;
import org.ff4j.audit.EventQueryDefinition;

import com.datastax.driver.core.Statement;
import com.datastax.driver.core.querybuilder.QueryBuilder;
/**
 * Helper to create query in Cassandra.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class CassandraQueryBuilder {
    
    /** Pattern for date in Cassandra. */
    public static final SimpleDateFormat SDF_TIMESTAMP = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
    
    /** Connection. */
    private final CassandraConnection connection;
    
    /**
     * Initialization of the builder with a dedicated connection.
     *
     * @param conn
     *      current cassandra collection.
     */
    public CassandraQueryBuilder(CassandraConnection conn) {
        this.connection = conn;
    }
    
    public String cqlDropAudit() {
        return "DROP TABLE IF EXISTS "+ connection.getKeySpace() + "." + COLUMN_FAMILY_AUDIT;
    }
    
    public String cqlDropFeatures() {
        return "DROP TABLE IF EXISTS "+ connection.getKeySpace() + "." + COLUMN_FAMILY_FEATURES;
    }
    
    public String cqlDropProperties() {
        return "DROP TABLE IF EXISTS "+ connection.getKeySpace() + "." + COLUMN_FAMILY_PROPERTIES;
    }
    
    /**
     * Generate expected CQL to create to column family Features.
     *
     * @param conn
     *      current connection (with keyspace name).
     * @return
     *      cql query.
     */
    public String cqlCreateColumnFamilyFeature() {
        return "CREATE TABLE " + connection.getKeySpace() + "." + 
                COLUMN_FAMILY_FEATURES + " ( " + 
                COL_FEAT_UID + " varchar, "   + 
                COL_FEAT_ENABLE + " int, " +
                COL_FEAT_DESCRIPTION + " varchar, "   +
                COL_FEAT_STRATEGY + " varchar, "   +
                COL_FEAT_GROUPNAME + " varchar, " + 
                COL_FEAT_ROLES + " set<varchar>, " + 
                COL_FEAT_CUSTOMPROPERTIES + " map<varchar,varchar>, " + 
                "PRIMARY KEY (" + COL_FEAT_UID + ")" +
                ");";
    }
    
    public String cqlCreateIndexGroupName() {
        return "CREATE INDEX ON " + connection.getKeySpace() + "." +
                COLUMN_FAMILY_FEATURES + " ( " + 
                COL_FEAT_GROUPNAME + ");";
    }

    public String cqlCreateFeature() {
        return "INSERT INTO " + connection.getKeySpace() + "." +
                COLUMN_FAMILY_FEATURES + "(" + 
                COL_FEAT_UID            + ", " + 
                COL_FEAT_ENABLE         + ", " +
                COL_FEAT_DESCRIPTION    + ", " +
                COL_FEAT_STRATEGY       + ", " +
                COL_FEAT_GROUPNAME      + ", " + 
                COL_FEAT_ROLES          + ", " + 
                COL_FEAT_CUSTOMPROPERTIES + ") " +
                "VALUES(?, ?, ?, ?, ?, ? ,?)";
    }
    
    public String cqlExistFeature() {
        return "SELECT COUNT(*) AS NB FROM " + connection.getKeySpace() + "." + 
                COLUMN_FAMILY_FEATURES + " WHERE " + COL_FEAT_UID + " = ?";
    }
    
    public String cqlEnableFeature() {
        return "UPDATE " + connection.getKeySpace() + "." + COLUMN_FAMILY_FEATURES + 
               " SET " + COL_FEAT_ENABLE + "=1" +
               " WHERE " + COL_FEAT_UID + " = ?";
    }
    
    public String cqlDisableFeature() {
        return "UPDATE " + connection.getKeySpace() + "." + COLUMN_FAMILY_FEATURES + 
               " SET " + COL_FEAT_ENABLE + "=0" +
               " WHERE " + COL_FEAT_UID + " = ?";
    }
    
    public String cqlDeleteFeature() {
        return "DELETE FROM " + connection.getKeySpace() + "." + COLUMN_FAMILY_FEATURES + 
               " WHERE " + COL_FEAT_UID + " = ?";
    }
    
    public String cqlReadFeature() {
        return "SELECT * FROM " + connection.getKeySpace() + "." + COLUMN_FAMILY_FEATURES + 
               " WHERE " + COL_FEAT_UID + " = ?";
    }
    
    public String cqlTruncateFeatures() {
        return "TRUNCATE TABLE " + connection.getKeySpace() + "." + COLUMN_FAMILY_FEATURES;
    }
    
    public Statement selectAllFeatures() {
        return QueryBuilder.select().all().from(connection.getKeySpace(), COLUMN_FAMILY_FEATURES);
    }
    
    public String cqlGrantRoleOnFeature(String roleName) {
        return "UPDATE "  + connection.getKeySpace() + "." + COLUMN_FAMILY_FEATURES + 
                " SET "   + COL_FEAT_ROLES + " = " + COL_FEAT_ROLES + " + {'" + roleName + "'}" +
                " WHERE " + COL_FEAT_UID + " = ?";
    }
    
    public String cqlReadFeatureRoles() {
        return "SELECT " + COL_FEAT_ROLES + " FROM " + connection.getKeySpace() + "." + COLUMN_FAMILY_FEATURES + 
                " WHERE " + COL_FEAT_UID + " = ?";
    }
    
    public String cqlUpdateFeatureRoles() {
        return "UPDATE "  + connection.getKeySpace() + "." + COLUMN_FAMILY_FEATURES + 
                " SET "   + COL_FEAT_ROLES + " = ?" + 
                " WHERE " + COL_FEAT_UID + " = ?";
    }
    
    public String cqlGetFeaturesNamesOfAGroup() {
        return "SELECT "  + COL_FEAT_UID + 
               " FROM " + connection.getKeySpace() + "." + COLUMN_FAMILY_FEATURES + 
               " WHERE " + COL_FEAT_GROUPNAME + "=?";
    }
    
    public String cqlGetFeaturesOfAGroup() {
        return "SELECT * FROM " + connection.getKeySpace() + "." + COLUMN_FAMILY_FEATURES + 
               " WHERE " + COL_FEAT_GROUPNAME + "=?";
    }
    
    public String cqlExistGroup() {
        return "SELECT COUNT(*) FROM " + connection.getKeySpace() + "." + COLUMN_FAMILY_FEATURES + 
               " WHERE " + COL_FEAT_GROUPNAME + "=?";
    }
    
    public String cqlAddFeatureToGroup() {
        return " UPDATE "  + connection.getKeySpace() + "." + COLUMN_FAMILY_FEATURES + 
                " SET " + COL_FEAT_GROUPNAME + "=?" +
                " WHERE " + COL_FEAT_UID + " = ?";
    }
    
    public String cqlRemoveFeatureFromGroup() {
        return "DELETE " + COL_FEAT_GROUPNAME +
                " FROM " + connection.getKeySpace() + "." + COLUMN_FAMILY_FEATURES + 
                " WHERE " + COL_FEAT_UID + " = ?";
    }
    
    public String cqlGetGroups() {
        return "SELECT " + COL_FEAT_GROUPNAME +
               " FROM " + connection.getKeySpace() + "." + COLUMN_FAMILY_FEATURES;
    }
    
    // --------------- Properties ---------------
    
    public String cqlTruncateProperties() {
        return "TRUNCATE TABLE " + connection.getKeySpace() + "." + COLUMN_FAMILY_PROPERTIES;
    }
    
    public String cqlCreateColumnFamilyProperties() {
        return "CREATE TABLE " + connection.getKeySpace() + "." + 
                COLUMN_FAMILY_PROPERTIES + " ( " + 
                COL_PROPERTY_ID             + " varchar, "    + 
                COL_PROPERTY_CLAZZ          + " varchar, "  +
                COL_PROPERTY_VALUE          + " varchar, "  +
                COL_PROPERTY_FIXED          + " set<varchar>, "  +
                COL_PROPERTY_DESCRIPTION    + " varchar, "  +
                "PRIMARY KEY (" + COL_PROPERTY_ID + ")" +
                ");";
    }
    
    public String cqlExistProperty() {
        return "SELECT COUNT(*) AS NB" + 
               " FROM " + connection.getKeySpace() + "." + COLUMN_FAMILY_PROPERTIES + 
               " WHERE " + COL_PROPERTY_ID + " = ?";
    }
    
    public String cqlCreateProperty() {
        return "INSERT INTO " + connection.getKeySpace() + "." +
                COLUMN_FAMILY_PROPERTIES   + "(" + 
                COL_PROPERTY_ID            + ", " + 
                COL_PROPERTY_CLAZZ         + ", " +
                COL_PROPERTY_VALUE         + ", " +
                COL_PROPERTY_DESCRIPTION   + ", " +
                COL_PROPERTY_FIXED         + ") " +
                "VALUES(?, ?, ?, ?, ?)";
    }
    
    public String cqlReadProperty() {
        return "SELECT * FROM " + connection.getKeySpace() + "." + COLUMN_FAMILY_PROPERTIES + 
               " WHERE " + COL_PROPERTY_ID + " = ?";
    }
    
    public String cqlDeleteProperty() {
        return "DELETE FROM " + connection.getKeySpace() + "." + COLUMN_FAMILY_PROPERTIES + 
               " WHERE " + COL_PROPERTY_ID + " = ?";
    }

    public Statement selectAllProperties() {
        return QueryBuilder.select().all().from(connection.getKeySpace(), COLUMN_FAMILY_PROPERTIES);
    }
    
    public String cqlPropertyNames() {
        return "SELECT " + COL_PROPERTY_ID + 
                " FROM " + connection.getKeySpace() + "." + COLUMN_FAMILY_PROPERTIES;
    }
    
    // ----- Audit
    
    public String cqlCreateColumnFamilyAudit() {
        return "CREATE TABLE " + connection.getKeySpace() + "." + 
                COLUMN_FAMILY_AUDIT + " ( " + 
                COL_EVENT_UID       + " varchar, "      + 
                COL_EVENT_DATE      + " varchar, "      +
                COL_EVENT_TIME      + " timestamp, "    +
                COL_EVENT_TYPE      + " varchar, "      +
                COL_EVENT_NAME      + " varchar, "      +
                COL_EVENT_ACTION    + " varchar, "      +
                COL_EVENT_HOSTNAME  + " varchar, "      +
                COL_EVENT_SOURCE    + " varchar, "      +
                COL_EVENT_DURATION  + " bigint, "      +
                COL_EVENT_USER      + " varchar, "      +
                COL_EVENT_VALUE     + " varchar, "      +
                COL_EVENT_KEYS      + " map<varchar,varchar>, "      +
                //"PRIMARY KEY ((" + COL_EVENT_NAME + "," + COL_EVENT_TYPE + "," + COL_EVENT_DATE +")," + COL_EVENT_TIME + ")) " +
                //"WITH CLUSTERING ORDER BY (" +  COL_EVENT_TIME + " DESC);";
                "PRIMARY KEY (" + COL_EVENT_UID  +")) ";
    }
    
    public String cqlCreateEvent(int ttl) {
        String query = "INSERT INTO " + connection.getKeySpace() + "." +
                COLUMN_FAMILY_AUDIT + "("  + 
                COL_EVENT_UID       + ", " + 
                COL_EVENT_DATE      + ", " +
                COL_EVENT_TIME      + ", " +
                COL_EVENT_TYPE      + ", " +
                COL_EVENT_NAME      + ", " +
                COL_EVENT_ACTION    + ", " +
                COL_EVENT_HOSTNAME  + ", " +
                COL_EVENT_SOURCE    + ", " +
                COL_EVENT_DURATION  + ", " +
                COL_EVENT_USER      + ", " +
                COL_EVENT_VALUE     + ", " +
                COL_EVENT_KEYS      + " )" +
                "VALUES(?, ?, ?, ?, ?, ? ,?, ?, ?, ?, ?, ?)";
        if (ttl > 0) {
            query += " USING TTL " + ttl;
        }
        return query;
    }
    
    public String cqlGetEventById() {
      return "SELECT * FROM " + connection.getKeySpace() + "." + COLUMN_FAMILY_AUDIT + 
              " WHERE " + COL_EVENT_UID + " = ? ALLOW FILTERING";
    }
    
    /**
     * Get uids
     * -> between 2 dates
     * -> For features only
     * -> for check only
     * @return
     */
    public String cqlFeatureUsageHitCount(EventQueryDefinition qDef) {
        qDef.getActionFilters().add(ACTION_CHECK_OK);
        return cqlSearchAudit(qDef, COL_EVENT_NAME, EventConstants.TARGET_FEATURE);
    }
    
    public String cqlUserHitCount(EventQueryDefinition qDef) {
        qDef.getActionFilters().add(ACTION_CHECK_OK);
        return cqlSearchAudit(qDef, COL_EVENT_USER, EventConstants.TARGET_FEATURE);
    }
    
    public String cqlHostHitCount(EventQueryDefinition qDef) {
        qDef.getActionFilters().add(ACTION_CHECK_OK);
        return cqlSearchAudit(qDef, COL_EVENT_HOSTNAME, EventConstants.TARGET_FEATURE);
    }
    
    public String cqlSourceHitCount(EventQueryDefinition qDef) {
        qDef.getActionFilters().add(ACTION_CHECK_OK);
        return cqlSearchAudit(qDef, COL_EVENT_SOURCE, EventConstants.TARGET_FEATURE);
    }
    
    public String cqlAuditTrail(EventQueryDefinition qDef) {
        return cqlSearchAudit(qDef, "*", null);
    }
    
    public String cqlAuditFeatureUsage(EventQueryDefinition qDef) {
        qDef.getActionFilters().add(ACTION_CHECK_OK);
        return cqlSearchAudit(qDef, "*", EventConstants.TARGET_FEATURE);
    }
    
    /**
     * Query audit to get audit events.
     *
     * @param qDef
     *      query definition
     * @param fields
     *      what to search in audit
     * @param filterAuditTrail
     *      filter to get audit
     * @param filterForCheck
     *      filter to get hitcount
     * @return
     *      CQL QUERY
     */
    private String cqlSearchAudit(EventQueryDefinition qDef, String fields, String type) {
        StringBuilder sb = new StringBuilder("SELECT " + fields);
        sb.append(" FROM "  + connection.getKeySpace() + "." + COLUMN_FAMILY_AUDIT); 
        sb.append(" WHERE (" + COL_EVENT_TIME + "> '" + SDF_TIMESTAMP.format(new Date(qDef.getFrom())) + "')");
        sb.append("   AND (" + COL_EVENT_TIME + "< '" + SDF_TIMESTAMP.format(new Date(qDef.getTo()))   + "')");
        if (null != type) {
            sb.append(" AND (" + COL_EVENT_TYPE + " = '" + type + "')");
        }
        // Filters
        if (!qDef.getActionFilters().isEmpty()) {
            sb.append(" AND (" + COL_EVENT_ACTION + " IN ");
            sb.append(buildClauseIn(qDef.getActionFilters()));
            sb.append(")");
        }
        if (!qDef.getHostFilters().isEmpty()) {
            sb.append(" AND (" + COL_EVENT_HOSTNAME + " IN ");
            sb.append(buildClauseIn(qDef.getHostFilters()));
            sb.append(")");
        }
        if (!qDef.getNamesFilter().isEmpty()) {
            sb.append(" AND (" + COL_EVENT_NAME + " IN ");
            sb.append(buildClauseIn(qDef.getNamesFilter()));
            sb.append(")");
        }
        if (!qDef.getSourceFilters().isEmpty()) {
            sb.append(" AND (" + COL_EVENT_SOURCE + " IN ");
            sb.append(buildClauseIn(qDef.getSourceFilters()));
            sb.append(")");
        }
        sb.append(" ALLOW FILTERING");
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
    
    public String cqlTruncateAudit() {
        return "TRUNCATE TABLE " + connection.getKeySpace() + "." + COLUMN_FAMILY_AUDIT;
    }
    
}
