package org.ff4j.jdbc;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import static org.ff4j.test.AssertUtils.assertNotEmpty;
import static org.ff4j.test.AssertUtils.assertNotNull;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.ff4j.jdbc.JdbcConstants.AuditTrailColumns;
import org.ff4j.jdbc.JdbcConstants.FeaturePropertyColumns;
import org.ff4j.jdbc.JdbcConstants.FeatureStrategyColumns;
import org.ff4j.jdbc.JdbcConstants.FeatureUsageColumns;
import org.ff4j.jdbc.JdbcConstants.FeaturesColumns;
import org.ff4j.jdbc.JdbcConstants.PermissionsColumns;
import org.ff4j.jdbc.JdbcConstants.PropertyColumns;
import org.ff4j.jdbc.JdbcConstants.PropertyPropertyColumns;
import org.ff4j.jdbc.JdbcConstants.RolesColumns;
import org.ff4j.jdbc.JdbcConstants.SQLTypes;
import org.ff4j.jdbc.JdbcConstants.SqlTableColumns;
import org.ff4j.jdbc.JdbcConstants.UserColumns;
import org.ff4j.jdbc.JdbcConstants.UserRoleAssociationColumns;

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
	
	/** Won't be generated each time. */
	private Map < String, String > insertIntoQueries = new HashMap<>();

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
	
    // ---------------------------------
    // ----------- Utilities -----------
    // ---------------------------------
	
	/**
	 * Generate the table name as you can provide both prefix and suffix.
	 *
	 * @param coreName
	 *         root of the tableName, will be 'prefix' + root + 'suffix'
	 * @return
	 *         the final table name
	 */
	protected String getTableName(String coreName) {
		return tablePrefix + coreName + tableSuffix;
	}

	/**
     * Generate an sql query to drop one Table.
     *
     * @param tableName
     *      target table name
     * @return
     *      the sql query
     */
    protected String sqlDropTable(String tableName) {
        StringBuilder sb = new StringBuilder("DROP TABLE ");
        sb.append(getTableName(tableName));
        sb.append(";\n");
        return sb.toString();
    }
    
    /**
     * Generate an sql query to create a table based on tableName.
     *
     * @param columns
     *      target columns
     * @return
     *      the sql query
     */
    public String sqlCreateTable(SqlTableColumns... columns) {
        assertNotEmpty(columns);
        SqlTableColumns tableColumn = columns[0];
        StringBuilder sb = new StringBuilder("CREATE TABLE ");
        sb.append(getTableName(tableColumn.tableName()));
        sb.append(" ( \n");
        Arrays.stream(columns).forEach(col -> { 
            sb.append(" " + col.colname() + " \t" + col.type().name());
            if (col.size() !=0 ) {
                sb.append("(" + col.size() + ")");
            }
            if (!col.nullable()) {
                sb.append(" NOT NULL");
            }
            sb.append(",\n");
        });
        sb.append(" PRIMARY KEY ");
        sb.append(tableColumn.primaryKey()
                    .stream().map(SqlTableColumns::colname)
                    .collect(Collectors.joining(",","(",")")));
        
        tableColumn.foreignKey().ifPresent(map -> {
            map.entrySet().stream().forEach(entry -> {
                sb.append(",\n FOREIGN KEY (");
                sb.append(entry.getKey().colname());
                sb.append(") REFERENCES ");
                sb.append(getTableName(entry.getValue().tableName()));
                sb.append("(" + entry.getValue().colname() + ")");
            });
        });
        sb.append("\n);");
        return sb.toString();
    }
  
    /**
     * Create a sql query to insert an element in table. The tableName is set by one of the colum.tableName().
     *
     * @param columns
     *      list of columns
     * @return
     *      the sql query
     */
    private String sqlInsert(SqlTableColumns... columns) {
        assertNotEmpty(columns);
        String tableName = getTableName(columns[0].tableName());
        if (!insertIntoQueries.containsKey(tableName)) {
            StringBuilder sb = new StringBuilder().append("INSERT INTO ");
            sb.append(tableName);
            sb.append(Arrays.stream(columns)
                        .map(SqlTableColumns::colname)
                        .collect(Collectors.joining(",","(\n",")\n")));
            sb.append(" VALUES");
            sb.append(IntStream.range(0, columns.length)
                        .mapToObj(i-> '?').map(o-> o.toString())
                        .collect(Collectors.joining(",","(",")")));
            insertIntoQueries.put(tableName, sb.toString());
        }
        return insertIntoQueries.get(tableName);
    }
    
    /**
     * Create a sql query to select columns in table. The tableName is set by one of the colum.tableName().
     *
     * @param distinct
     *      if the marqer 'distinct' sould be added
     * @param columns
     *      the list of columns to retrieve, could
     * @return
     *      the sql query
     */
    private String sqlSelect(boolean distinct, SqlTableColumns... columns) {
        assertNotEmpty(columns);
        SqlTableColumns tableColumn = columns[0];
        return new StringBuilder("SELECT ")
                .append(distinct ? "DISTINCT (" : "")
                .append(Arrays.stream(columns).map(SqlTableColumns::colname).collect(Collectors.joining(",")))
                .append(distinct ? ")" : "")
                .append(" FROM ")
                .append(getTableName(tableColumn.tableName())).toString();
    }
    
    
    /**
     * Select element in sql query.
     *
     * @param distinct
     *          if the marqer 'distinct' sould be added
     * @param condition
     *          set the condition for a column
     * @param columns
     *          list of columns to retrieve (select clause)
     * @return
     *          element to retrieve
     */
    protected String sqlSelectWhere(boolean distinct, SqlTableColumns condition, SqlTableColumns... columns) {
        return sqlSelect(distinct, columns) + sqlPartWhere(condition);
    }
    
    /**
     * Generate the 'where' instruction as concatenation of smaller conditions.
     *
     * @param condition
     *      list of columns to create where conditions
     * @return
     *      sql query
     */
    private String sqlPartWhere(SqlTableColumns... condition) {
        if (condition == null || condition.length == 0) return "";
        StringBuilder sb =  new StringBuilder(" WHERE " + sqlPartWhereCondition(condition[0]));
        for (int i = 1; i < condition.length; i++) {
            sb.append(" AND ").append(sqlPartWhereCondition(condition[i]));
        }
        return sb.toString();
    }
    
    /**
     * Generate 'part' of the condition where.
     *
     * @param condition
     *      cloumn on which perform the clause.
     * @return
     *      part of the 
     */
    private String sqlPartWhereCondition(SqlTableColumns condition) {
        StringBuilder sb = new StringBuilder("(");
        sb.append(condition.colname());
        sb.append(condition.type().equals(SQLTypes.VARCHAR) ? " LIKE ?" : " = ?");
        return sb.append(")").toString();
    }
    
    // --- Count ---
    
    private String sqlCount(SqlTableColumns column) {
        return "SELECT COUNT(" + column.colname() + ") FROM " + getTableName(column.tableName());
    }
    
    private String sqlCountWhere(SqlTableColumns column, SqlTableColumns condition) {
        return sqlCount(column) + sqlPartWhere(condition);
    }
    
    // ---------------------------------
    // ----------- Schema    -----------
    // ---------------------------------
    
    /** All SQL Script. */
    public String sqlDropSchema() {
        return new StringBuilder()
                // Security tables
                .append(sqlDropTable(UserRoleAssociationColumns.REF_USER.tableName()))
                .append(sqlDropTable(RolesColumns.NAME.tableName()))
                .append(sqlDropTable(UserColumns.UID.tableName()))
                .append(sqlDropTable(PermissionsColumns.PERMISSION.tableName()))
                // Features Tables
                .append(sqlDropTable(FeatureStrategyColumns.FEATURE.tableName()))
                .append(sqlDropTable(FeaturePropertyColumns.FEATURE.tableName()))
                .append(sqlDropTable(FeaturesColumns.UID.tableName()))
                // Properties Tables (single strategy included in Property table)
                .append(sqlDropTable(PropertyPropertyColumns.PROPERTY.tableName()))
                .append(sqlDropTable(PropertyColumns.UID.tableName()))
                // Audit Tables
                .append(sqlDropTable(FeatureUsageColumns.UID.tableName()))
                .append(sqlDropTable(AuditTrailColumns.UID.tableName()))
                .toString();
    }
    
    /** All SQL Script. */
    public String sqlCreateSchema() {
        return new StringBuilder()
                .append(sqlCreateTable(FeaturesColumns.values()))
                .append("\n")
                .append(sqlCreateTable(FeaturePropertyColumns.values()))
                .append("\n")
                .append(sqlCreateTable(FeatureStrategyColumns.values()))
                .append("\n")
                .append(sqlCreateTable(PropertyColumns.values()))
                .append("\n")
                .append(sqlCreateTable(PropertyPropertyColumns.values()))
                .append("\n")
                .append(sqlCreateTable(AuditTrailColumns.values()))
                .append("\n")
                .append(sqlCreateTable(FeatureUsageColumns.values()))
                .append("\n")
                .append(sqlCreateTable(PermissionsColumns.values()))
                .append("\n")   
                .append(sqlCreateTable(UserColumns.values()))
                .append("\n")
                .append(sqlCreateTable(RolesColumns.values()))
                .append("\n")
                .append(sqlCreateTable(UserRoleAssociationColumns.values()))
                .append("\n")
                .toString();
    }
    
	// ---------------------------------
    // -- TABLE Features              --
	// ---------------------------------
    
	public String getTableNameFeatures() {
        return getTableName(FeaturesColumns.UID.tableName());
    }
	
	public String sqlCreateTableFeatures() {
        return sqlCreateTable(FeaturesColumns.values());
    }
	
	public String sqlDropTableFeatures() {
        return sqlDropTable(FeaturesColumns.UID.tableName());
    }
	
	public String sqlFindAllFeatures() {
        return sqlSelect(false, FeaturesColumns.values());
    }
	
	public String sqlFindFeatureById() {
	   // distinct flag | where condition column | select columns
       return sqlSelectWhere(false, FeaturesColumns.UID, FeaturesColumns.values());
    }
	
	public String sqlInsertFeature() {
        return sqlInsert(FeaturesColumns.values());
    }
	
	public String sqlSelectAllGroups() {
	    return sqlSelect(true, FeaturesColumns.GROUPNAME);
	}
	
	// ---------------------------------
    // -- TABLE Features_Strategy     --
    // ---------------------------------

	public String getTableNameFeatureStrategy() {
        return getTableName(FeatureStrategyColumns.FEATURE.tableName());
    }

	public String sqlCreateTableToggleStrategy() {
	    return sqlCreateTable(FeatureStrategyColumns.values());
	}
	
	public String sqlDropTableToggleStrategy() {
        return sqlDropTable(FeatureStrategyColumns.FEATURE.tableName());
    }
	
	public String sqlInsertToggleStrategy() {
	    return sqlInsert(FeatureStrategyColumns.values());
	}
	 
    public String sqlStrategyOfFeature() {
        return sqlSelectWhere(false, FeatureStrategyColumns.FEATURE, FeatureStrategyColumns.values());
    }
    
    // ---------------------------------
    // -- TABLE Features_Properties  --
    // ---------------------------------

	public String getTableNameFeatureProperties() {
	    return getTableName(FeaturePropertyColumns.FEATURE.tableName());
	}
	public String sqlCreateTableFeatureProperties() {
        return sqlCreateTable(FeaturePropertyColumns.values());
    }
    public String sqlDropTableFeatureProperties() {
        return sqlDropTable(FeaturePropertyColumns.UID.tableName());
    }
    
    /** Roles for a feature. */
    public String sqlSelectCustomPropertiesOfFeature() {
        return sqlSelectWhere(false, 
                FeaturePropertyColumns.UID, FeaturePropertyColumns.values());
    }
    
    /** Roles for a feature. */
    public String sqlSelectCustomPropertyOfFeature() {
        return sqlSelect(false, FeaturePropertyColumns.values()) + 
                sqlPartWhere(FeaturePropertyColumns.UID) + " AND " +
                FeaturePropertyColumns.UID + " = ?";
    }
    
    // ---------------------------------
    // -- TABLE Properties            --
    // ---------------------------------

    public String getTableNameProperties() {
        return getTableName(PropertyColumns.UID.tableName());
    }
    
    public String sqlCreateTableProperties() {
        return sqlCreateTable(PropertyColumns.values());
    }
    
    public String sqlDropTableProperties() {
        return sqlDropTable(PropertyColumns.UID.tableName());
    }
    
    // ---------------------------------
    // -- TABLE Permissions           --
    // ---------------------------------
    
    public String getTableNamePermissions() {
        return getTableName(PermissionsColumns.PERMISSION.tableName());
    }
    
    public String sqlDropTablePermissions() {
        return sqlDropTable(PermissionsColumns.PERMISSION.tableName());
    }
    
    public String sqlCreateTablePermissions() {
        return sqlCreateTable(PermissionsColumns.values());
    }
    
    public String sqlSelectglobalPermissions() {
        return sqlSelect(false, PermissionsColumns.values());
    }
    
    // ---------------------------------
    // -- TABLE USERS                 --
    // ---------------------------------
    
    public String getTableNameUser() {
        return getTableName(UserColumns.UID.tableName());
    }
    
    public String sqlDropTableUser() {
        return sqlDropTable(UserColumns.UID.tableName());
    }
    
    public String sqlCreateTableUser() {
        return sqlCreateTable(UserColumns.values());
    }
    
    public String sqlSelectUsers() {
        return sqlSelect(false, UserColumns.values());
    }
    
    // ---------------------------------
    // -- TABLE ROLES                 --
    // ---------------------------------
    
    public String getTableNameRoles() {
        return getTableName(RolesColumns.NAME.tableName());
    }
    
    public String sqlDropTableRoles() {
        return sqlDropTable(RolesColumns.NAME.tableName());
    }
    
    public String sqlCreateTableRoles() {
        return sqlCreateTable(RolesColumns.values());
    }
    
    public String sqlSelectAllRoles() {
        return sqlSelect(false, RolesColumns.values());
    }
    
    // ---------------------------------
    // -- TABLE ROLE_USE              --
    // ---------------------------------
    
    public String getTableNameRolesUsers() {
        return getTableName(UserRoleAssociationColumns.REF_USER.tableName());
    }
    
    public String sqlDropTableRolesUsers() {
        return sqlDropTable(UserRoleAssociationColumns.REF_USER.tableName());
    }
    
    public String sqlCreateTableRolesUsers() {
        return sqlCreateTable(UserRoleAssociationColumns.values());
    }
    
    public String sqlSelectAllRolesUsers() {
        return sqlSelect(false, UserRoleAssociationColumns.values());
    }
    
    
    // ---------------------------------
    // -- TABLE Features_Usage        --
    // ---------------------------------

    public String getTableNameFeatureUsage() {
        return getTableName(UserRoleAssociationColumns.REF_USER.tableName());
    }
    // ---------------------------------
    
    /** Get all features. */
    public String sqlSelectAllCustomProperties() {
        return sqlSelect(false, FeaturePropertyColumns.values());
    }
    
    /** Get all features. */
    public String sqlSelectFeaturesOfGroup() {
        return sqlSelectWhere(false, FeaturesColumns.GROUPNAME, FeaturesColumns.values());
    }
    
    // 04
   
    
    // 05
    
    
    // 07
    public String getTableNamePropertiesProperty() {
        return getTableName(PropertyPropertyColumns.PROPERTY.tableName());
    }
    
    // 09
    public String getTableNameUsers() {
        return getTableName(UserColumns.UID.tableName());
    }
    
    // 10
    
    
    // 11
    public String getTableNameUserRolesAssociation() {
        return getTableName(UserRoleAssociationColumns.REF_USER.tableName());
    }
    
    // 12
    public String getTableNameAuditTrail() {
        return getTableName(AuditTrailColumns.UID.tableName());
    }
    
    // 13
    
    
    public String sqlSelectFeatureAccessControlList() {
        return null;
    }
    
    
    
    public String sqlInsertAuditTrail() {
        return sqlInsert(AuditTrailColumns.values());
    }
    
    public String sqlInsertMetrics() {
        return sqlInsert(FeatureUsageColumns.values());
    }
    
    public String sqlInsertProperty() {
        return sqlInsert(PropertyColumns.values());
    }
    
    public String sqlInsertCustomProperties() {
        return sqlInsert(FeaturePropertyColumns.values());
    }
    
    
    
    
    // ----- Properties -----
    
    /** Get all properties. */
    public String sqlSelectAllProperties() {
        return sqlSelect(false, PropertyColumns.values());
    }
    
    /** Check if property exist. */
    public String sqlExistProperty() {
        return sqlCountWhere(PropertyColumns.UID, PropertyColumns.UID);
    }
    
    /** Get an event by its id. */
    public String sqlSelectPropertyById() {
        return sqlSelectWhere(false, PropertyColumns.UID, PropertyColumns.values());
    }
    
    /** Get all property names. */
    public String sqlSelectAllPropertyNames() {
        return sqlSelect(true, PropertyColumns.UID);
    }
    
    /** Get an event by its id. */
    public String sqlSelectAuditById() {
        return sqlSelectWhere(false, AuditTrailColumns.UID, AuditTrailColumns.values());
    }
    
    // ----- Features -----
    
    /** Count Features. */
    public String sqlCountFeatures() {
        return sqlCount(FeaturesColumns.UID);
    }
    
    /** Check if feature exist. */
    public String sqlExistFeature() {
        return sqlCountWhere(FeaturesColumns.UID, FeaturesColumns.UID);
    }
    
    /** Check if feature exist. */
    public String sqlExistGroup() {
        return sqlCountWhere(FeaturesColumns.UID, FeaturesColumns.GROUPNAME);
    }
    
    // ---------------------------------
    // -------     UPDATE    -----------
    // ---------------------------------
    
    /** Update a table . */
    private String sqlUpdate(SqlTableColumns condition, SqlTableColumns... tobeUpdated) {
        assertNotNull(condition);
        assertNotEmpty(tobeUpdated);
        StringBuilder fields = new StringBuilder();
        Arrays.stream(tobeUpdated).map(SqlTableColumns::colname).forEach(colname -> {
            fields.append(" AND " + colname + " = ?");
        });
        return "UPDATE " + getTableName(condition.tableName()) + " SET " + fields.substring(4) + sqlPartWhere(condition);
    }
    
    public String updateProperty() {
        StringBuilder sb = new StringBuilder();
        sb.append("UPDATE ");
        sb.append(getTableNameProperties());
        sb.append(" SET CURRENTVALUE = ? WHERE PROPERTY_ID = ?");
        return sb.toString();
    }
    
    // ----- Features -----
    
    /** Enable a feature. */
    public String sqlEditFeatureStatus() {
        return sqlUpdate(FeaturesColumns.UID, FeaturesColumns.ENABLE);
	}
    
    /** Enable a group. */
    public String sqlEditGroupStatus() {
        return sqlUpdate(FeaturesColumns.GROUPNAME, FeaturesColumns.ENABLE);
    }
    
    /** Update group name for dedicated feature uid. */
	public String sqlEditFeatureToGroup() {
	    return sqlUpdate(FeaturesColumns.UID, FeaturesColumns.GROUPNAME);
	}
	
	// ---------------------------------
    // -------     DELETE    -----------
    // ---------------------------------
 
    private String sqlDeleteAll(SqlTableColumns column) {
        assertNotNull(column);
        StringBuilder sb =  new StringBuilder().append("DELETE FROM ");
        sb.append(getTableName(column.tableName()));
        return sb.toString();
    }
    
    private String sqlDeleteWhere(SqlTableColumns... condition) {
        return sqlDeleteAll(condition[0]) + sqlPartWhere(condition);
    }

    public String sqlDeleteFeature() {
        return sqlDeleteWhere(FeaturesColumns.UID);
    }
    
    public String sqlDeleteAllFeatures() {
        return sqlDeleteAll(FeaturesColumns.UID);
    }
    
    public String sqlDeleteAllCustomPropertiesOfFeature() {
        return sqlDeleteWhere(FeaturePropertyColumns.UID);
    }
    
    public String sqlDeletePropertyOfFeature() {
        return sqlDeleteWhere(FeaturePropertyColumns.UID, FeaturePropertyColumns.UID);
    }
    
    public String sqlDeleteAllCustomProperties() {
        return sqlDeleteAll(FeaturePropertyColumns.UID);
    }
    
    public String sqlDeleteProperty() {
        return sqlDeleteWhere(PropertyColumns.UID);
    }
    
    public String sqlDeleteAllProperties() {
        return sqlDeleteAll(PropertyColumns.UID);
    }
    
    public String sqlDeleteAuditEvent() {
        return sqlDeleteWhere(AuditTrailColumns.UID);
    }
}
