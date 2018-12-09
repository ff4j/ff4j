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
import org.ff4j.jdbc.JdbcConstants.FeaturePermissionColumns;
import org.ff4j.jdbc.JdbcConstants.FeaturePropertyColumns;
import org.ff4j.jdbc.JdbcConstants.FeatureToggleStrategyColumns;
import org.ff4j.jdbc.JdbcConstants.FeatureToggleStrategyPropertiesColumns;
import org.ff4j.jdbc.JdbcConstants.FeatureUsageColumns;
import org.ff4j.jdbc.JdbcConstants.FeaturesColumns;
import org.ff4j.jdbc.JdbcConstants.PropertyColumns;
import org.ff4j.jdbc.JdbcConstants.RolesColumns;
import org.ff4j.jdbc.JdbcConstants.RolesPermissionColumns;
import org.ff4j.jdbc.JdbcConstants.SQLTypes;
import org.ff4j.jdbc.JdbcConstants.SqlTableColumns;
import org.ff4j.jdbc.JdbcConstants.UsersColumns;
import org.ff4j.jdbc.JdbcConstants.UsersPermissionColumns;
import org.ff4j.jdbc.JdbcConstants.UsersRolesColumns;

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
        sb.append("\n);\n");
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
                .append(sqlDropTableRolesUsers())
                .append(sqlDropTableUserPermission())
                .append(sqlDropTableUser())
                .append(sqlDropTableRolesPermissions())
                .append(sqlDropTableRoles())
                .append(sqlDropTableToggleStrategyProperties())
                .append(sqlDropTableToggleStrategy())
                .append(sqlDropTableFeaturePermission())
                .append(sqlDropTableFeatureProperties())
                .append(sqlDropTableFeatures())
                .append(sqlDropTableProperties())
                .append(sqlDropTableFeatureUsage())
                .append(sqlDropTableAuditTrail())
                .toString();
    }
    
    /** All SQL Script. */
    public String sqlCreateSchema() {
        return new StringBuilder()
                .append(sqlCreateTableRoles())
                .append(sqlCreateTableRolesPermissions())
                .append(sqlCreateTableUser())
                .append(sqlCreateTableUserPermissions())
                .append(sqlCreateTableRolesUsers())
                .append(sqlCreateTableFeatures())
                .append(sqlCreateTableFeatureProperties())
                .append(sqlCreateTableFeaturePermission())
                .append(sqlCreateTableToggleStrategy())
                .append(sqlCreateTableToggleStrategyProperties())
                .append(sqlCreateTableProperties())
                .append(sqlCreateTableAuditTrail())
                .append(sqlCreateTableFeatureUsage())
                .toString();
    }
    
	// ---------------------------------
    // -- TABLE Features              --
	// ---------------------------------
    
    // tablename
	public String getTableNameFeatures() {
        return getTableName(FeaturesColumns.UID.tableName());
    }
	 // create
    public String sqlCreateTableFeatures() {
        return sqlCreateTable(FeaturesColumns.values());
    }
    // drop 
    public String sqlDropTableFeatures() {
        return sqlDropTable(FeaturesColumns.UID.tableName());
    }
    // select *
    public String sqlFindAllFeatures() {
        return sqlSelect(false, FeaturesColumns.values());
    }
    // select by id
    public String sqlFindFeatureById() {
	   // distinct flag | where condition column | select columns
       return sqlSelectWhere(false, FeaturesColumns.UID, FeaturesColumns.values());
    }
    // insert
	public String sqlInsertFeature() {
        return sqlInsert(FeaturesColumns.values());
    }
	// delete
    public String sqlDeleteFeature() {
        return sqlDeleteWhere(FeaturesColumns.UID);
    }
    // delete all
    public String sqlDeleteAllFeatures() {
        return sqlDeleteAll(FeaturesColumns.UID);
    }
	// count
    public String sqlCountFeatures() {
        return sqlCount(FeaturesColumns.UID);
    }
    // exist
    public String sqlExistFeature() {
        return sqlCountWhere(FeaturesColumns.UID, FeaturesColumns.UID);
    }
    // enable
    public String sqlEditFeatureStatus() {
        return sqlUpdate(FeaturesColumns.UID, FeaturesColumns.ENABLE);
    }
    
    // -- Dedicated Group queries
    
    // exist group
    public String sqlExistGroup() {
        return sqlCountWhere(FeaturesColumns.UID, FeaturesColumns.GROUPNAME);
    }
    // select all groups
    public String sqlSelectAllGroups() {
        return sqlSelect(true, FeaturesColumns.GROUPNAME);
    }
    // enable group
    public String sqlEditGroupStatus() {
        return sqlUpdate(FeaturesColumns.GROUPNAME, FeaturesColumns.ENABLE);
    }
    // add/remove from groups
    public String sqlEditFeatureToGroup() {
        return sqlUpdate(FeaturesColumns.UID, FeaturesColumns.GROUPNAME);
    }
    // Get features of a group
    public String sqlSelectFeaturesOfGroup() {
        return sqlSelectWhere(false, FeaturesColumns.GROUPNAME, FeaturesColumns.values());
    }
    
	// ---------------------------------------
    // -- TABLE Feature_Perm                --
    // ---------------------------------------
	
	// tablename
    public String getTableNameFeaturePermission() {
	    return getTableName(FeaturePermissionColumns.FEAT_UID.tableName());
	}
	// create
    public String sqlCreateTableFeaturePermission() {
        return sqlCreateTable(FeaturePermissionColumns.values());
    }
    // drops
    public String sqlDropTableFeaturePermission() {
        return sqlDropTable(FeaturePermissionColumns.FEAT_UID.tableName());
    }
    // insert
    public String sqlInsertFeaturePermission() {
        return sqlInsert(FeaturePermissionColumns.values());
    }
    // delete target permission
    public String sqlDeleteFeaturePermission() {
        return sqlDeleteWhere(FeaturePermissionColumns.FEAT_UID, FeaturePermissionColumns.PERMISSION);
    }
    // delete all for a feature 
    public String sqlDeleteAllFeaturePermissionForFeature() {
        return sqlDeleteWhere(FeaturePermissionColumns.FEAT_UID);
    }
    // delete all 
    public String sqlDeleteAllFeaturePermission() {
        return sqlDeleteAll(FeaturePermissionColumns.FEAT_UID);
    }
    // select *
    public String sqlSelectAllFeaturePermissions() {
        return sqlSelect(false, FeaturePermissionColumns.values());
    }
    // select
    public String sqlSelectPermissionsOfFeature() {
        return sqlSelectWhere(false, FeaturePermissionColumns.FEAT_UID, FeaturePermissionColumns.values());
    }
    
 	// ---------------------------------------
    // -- TABLE Toggle_Strategy             --
    // ---------------------------------------
   
    // tablename
    public String getTableNameToggleStrategy() {
        return getTableName(FeatureToggleStrategyColumns.FEATURE_UID.tableName());
    }
    // create
    public String sqlCreateTableToggleStrategy() {
        return sqlCreateTable(FeatureToggleStrategyColumns.values());
    }
    // drop 
    public String sqlDropTableToggleStrategy() {
        return sqlDropTable(FeatureToggleStrategyColumns.FEATURE_UID.tableName());
    }
    // insert
    public String sqlInsertToggleStrategy() {
        return sqlInsert(FeatureToggleStrategyColumns.values());
    }
    // select *
    public String sqlSelectAllToggleStrategies() {
        return sqlSelect(false, FeatureToggleStrategyColumns.values());
    }
    // select with feature uid filter
    public String sqlSelectToggleStrategiesForFeature() {
        return sqlSelectWhere(false, FeatureToggleStrategyColumns.FEATURE_UID, 
                FeatureToggleStrategyColumns.values());
    }
    // delete target strategy
    public String sqlDeleteToggleStrategy() {
        return sqlDeleteWhere(FeatureToggleStrategyColumns.FEATURE_UID, FeatureToggleStrategyColumns.TOGGLE_CLASS);
    }
    // delete all for a feature 
    public String sqlDeleteAllToggleStrategieForFeature() {
        return sqlDeleteWhere(FeatureToggleStrategyColumns.FEATURE_UID);
    }
    // delete all 
    public String sqlDeleteAllToggleStrategies() {
        return sqlDeleteAll(FeatureToggleStrategyColumns.FEATURE_UID);
    }
    
    // ---------------------------------------
    // -- TABLE Toggle_Strategy_Properties  --
    // ---------------------------------------
    
    // tablename
    public String getTableNameToggleStrategyProperties() {
        return getTableName(FeatureToggleStrategyPropertiesColumns.UID.tableName());
    }
    // create
    public String sqlCreateTableToggleStrategyProperties() {
        SqlTableColumns tableColumn = FeatureToggleStrategyPropertiesColumns.UID;
        StringBuilder sb = new StringBuilder("CREATE TABLE ");
        sb.append(getTableName(tableColumn.tableName()));
        sb.append(" ( \n");
        Arrays.stream(FeatureToggleStrategyPropertiesColumns.values()).forEach(col -> { 
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
        sb.append(",\n");
        // Overriding to create a composite FK
        sb.append(" FOREIGN KEY (");
        sb.append(FeatureToggleStrategyPropertiesColumns.STRAT_FEAT_UID.colname() + ",");
        sb.append(FeatureToggleStrategyPropertiesColumns.STRAT_CLASS.colname());
        sb.append(") REFERENCES " + getTableNameToggleStrategy() + "(");
        sb.append(FeatureToggleStrategyColumns.FEATURE_UID.colname() + ",");
        sb.append(FeatureToggleStrategyColumns.TOGGLE_CLASS.colname() + ")\n);");
        return sb.toString();
    }
    // drop 
    public String sqlDropTableToggleStrategyProperties() {
        return sqlDropTable(FeatureToggleStrategyPropertiesColumns.UID.tableName());
    }
    // insert
    public String sqlInsertToggleStrategyProperties() {
        return sqlInsert(FeatureToggleStrategyPropertiesColumns.values());
    }
    // select *
    public String sqlSelectAllToggleStrategiesProperties() {
        return sqlSelect(false, FeatureToggleStrategyPropertiesColumns.values());
    }
    // select    
    public String sqlSelectToggleStrategiesPropOfFeature() {
        return sqlSelectWhere(false, FeatureToggleStrategyPropertiesColumns.STRAT_FEAT_UID, 
                FeatureToggleStrategyPropertiesColumns.values());
    }
    // delete
    public String sqlDeleteAllToggleStrategyPropertiesForFeature() {
        return sqlDeleteWhere(FeatureToggleStrategyPropertiesColumns.STRAT_FEAT_UID);
    }
    // delete for a toggle strategy
    public String sqlDeleteToggleStrategyPropertiesForToggleStrategy() {
        return sqlDeleteWhere(
                FeatureToggleStrategyPropertiesColumns.STRAT_FEAT_UID,
                FeatureToggleStrategyPropertiesColumns.STRAT_CLASS);
    }
    // delete
    public String sqlDeleteToggleStrategyProperty() {
        return sqlDeleteWhere(
                FeatureToggleStrategyPropertiesColumns.STRAT_FEAT_UID,
                FeatureToggleStrategyPropertiesColumns.STRAT_CLASS,
                FeatureToggleStrategyPropertiesColumns.UID);
    }
    // delete all 
    public String sqlDeleteAllToggleStrategyProperties() {
        return sqlDeleteAll(FeatureToggleStrategyPropertiesColumns.UID);
    }
    
    // ---------------------------------
    // -- TABLE Features_Properties  --
    // ---------------------------------
 
    // tablename
	public String getTableNameFeatureProperties() {
	    return getTableName(FeaturePropertyColumns.FEATURE.tableName());
	}
	// create
    public String sqlCreateTableFeatureProperties() {
        return sqlCreateTable(FeaturePropertyColumns.values());
    }
    // drop 
    public String sqlDropTableFeatureProperties() {
        return sqlDropTable(FeaturePropertyColumns.UID.tableName());
    }
    // insert
    public String sqlInsertProperty() {
        return sqlInsert(PropertyColumns.values());
    }
    // delete target property
    public String sqlDeletePropertyOfFeature() {
        return sqlDeleteWhere(FeaturePropertyColumns.UID, FeaturePropertyColumns.UID);
    }
    // delete all for a feature 
    public String sqlDeleteAllCustomProperties() {
        return sqlDeleteAll(FeaturePropertyColumns.UID);
    }
    // delete all
    public String sqlDeleteAllPropertiesFromFeature() {
        return sqlDeleteWhere(FeaturePropertyColumns.FEATURE);
    }
    // select *
    public String sqlSelectAllFeatureProperties() {
        return sqlSelect(false, FeaturePropertyColumns.values());
    }
    // select by feature uid
    public String sqlSelectPropertiesForFeature() {
        return sqlSelectWhere(false, FeaturePropertyColumns.FEATURE, FeaturePropertyColumns.values());
    }
    // select by feature uid and Property UID
    public String sqlSelectCustomPropertyOfFeature() {
        return sqlSelect(false, FeaturePropertyColumns.values()) + 
                sqlPartWhere(FeaturePropertyColumns.UID) + " AND " +
                FeaturePropertyColumns.UID + " = ?";
    }
    
    // ---------------------------------
    // -- TABLE Properties            --
    // ---------------------------------

    // tablename
    public String getTableNameProperties() {
        return getTableName(PropertyColumns.UID.tableName());
    }
    // create
    public String sqlCreateTableProperties() {
        return sqlCreateTable(PropertyColumns.values());
    }
    // drop
    public String sqlDropTableProperties() {
        return sqlDropTable(PropertyColumns.UID.tableName());
    }
    // select *
    public String sqlSelectAllProperties() {
        return sqlSelect(false, PropertyColumns.values());
    }
    // insert
    public String sqlInsertFeatureProperty() {
        return sqlInsert(FeaturePropertyColumns.values());
    }
    // delete
    public String sqlDeleteProperty() {
        return sqlDeleteWhere(PropertyColumns.UID);
    }
    // delete all
    public String sqlDeleteAllProperties() {
        return sqlDeleteAll(PropertyColumns.UID);
    }
    // exists
    public String sqlExistProperty() {
        return sqlCountWhere(PropertyColumns.UID, PropertyColumns.UID);
    }
    // select by ids
    public String sqlSelectPropertyById() {
        return sqlSelectWhere(false, PropertyColumns.UID, PropertyColumns.values());
    }
    // list all property name
    public String sqlSelectAllPropertyNames() {
        return sqlSelect(true, PropertyColumns.UID);
    }
    
    // ---------------------------------
    // -- TABLE USERS                 --
    // ---------------------------------
    
    // tablename
    public String getTableNameUser() {
        return getTableName(UsersColumns.UID.tableName());
    }
    // create
    public String sqlCreateTableUser() {
        return sqlCreateTable(UsersColumns.values());
    }
    // drop
    public String sqlDropTableUser() {
        return sqlDropTable(UsersColumns.UID.tableName());
    }
    // insert
    public String sqlInsertUser() {
        return sqlInsert(UsersColumns.values());
    }
    // select *
    public String sqlSelectUsers() {
        return sqlSelect(false, UsersColumns.values());
    }
    // select by ids
    public String sqlSelectUserById() {
        return sqlSelectWhere(false, UsersColumns.UID, UsersColumns.values());
    }
    // exists
    public String sqlExistUser() {
        return sqlCountWhere(UsersColumns.UID, UsersColumns.UID);
    }
    // delete
    public String sqlDeleteUser() {
        return sqlDeleteWhere(UsersColumns.UID);
    }
    // delete all
    public String sqlDeleteAllUsers() {
        return sqlDeleteAll(UsersColumns.UID);
    }
    
    // ---------------------------------
    // -- TABLE USERS_PERMISSIONS     --
    // ---------------------------------
    
    // tablename
    public String getTableNameUserPermissions() {
        return getTableName(UsersPermissionColumns.USER_UID.tableName());
    }
    // create
    public String sqlCreateTableUserPermissions() {
        return sqlCreateTable(UsersPermissionColumns.values());
    }
    // drops
    public String sqlDropTableUserPermission() {
        return sqlDropTable(UsersPermissionColumns.USER_UID.tableName());
    }
    // insert
    public String sqlInsertUserPermission() {
        return sqlInsert(UsersPermissionColumns.values());
    }
    // select *
    public String sqlSelectAllUserPermissions() {
        return sqlSelect(false, UsersPermissionColumns.values());
    }
    // select
    public String sqlSelectPermissionsOfUser() {
        return sqlSelectWhere(false, UsersPermissionColumns.USER_UID, UsersPermissionColumns.values());
    }
    // delete target user Permission
    public String sqlDeletePermissionOfUser() {
        return sqlDeleteWhere(UsersPermissionColumns.USER_UID, UsersPermissionColumns.PERMISSION);
    }
    // delete all for a feature 
    public String sqlDeleteAllPermissionsForUser() {
        return sqlDeleteWhere(UsersPermissionColumns.USER_UID);
    }
    // delete all 
    public String sqlDeleteAllUserPermissions() {
        return sqlDeleteAll(UsersPermissionColumns.USER_UID);
    }
    
    // ---------------------------------
    // -- TABLE ROLES                 --
    // ---------------------------------
    
    // tablename
    public String getTableNameRoles() {
        return getTableName(RolesColumns.NAME.tableName());
    }
    // create
    public String sqlCreateTableRoles() {
        return sqlCreateTable(RolesColumns.values());
    }
    // drop
    public String sqlDropTableRoles() {
        return sqlDropTable(RolesColumns.NAME.tableName());
    }
    // insert
    public String sqlInserRole() {
        return sqlInsert(RolesColumns.values());
    }
    // select *
    public String sqlSelectAllRoles() {
        return sqlSelect(false, RolesColumns.values());
    }
    // select by id
    public String sqlSelectRoleByName() {
        return sqlSelectWhere(false, RolesColumns.NAME, RolesColumns.values());
    }
    // exists
    public String sqlExisRole() {
        return sqlCountWhere(RolesColumns.NAME, RolesColumns.NAME);
    }
    // delete
    public String sqlDeleteRole() {
        return sqlDeleteWhere(RolesColumns.NAME);
    }
    // delete all
    public String sqlDeleteAllRoles() {
        return sqlDeleteAll(RolesColumns.NAME);
    }
    
    // --------------------------------
    // -- TABLE ROLES_PERMISSIONS    --
    // --------------------------------
    
    // tablename
    public String getTableNameRolesPermissions() {
        return getTableName(RolesPermissionColumns.ROLE_NAME.tableName());
    }
    // create
    public String sqlCreateTableRolesPermissions() {
        return sqlCreateTable(RolesPermissionColumns.values());
    }
    // drop
    public String sqlDropTableRolesPermissions() {
        return sqlDropTable(RolesPermissionColumns.ROLE_NAME.tableName());
    }
    // insert
    public String sqlInsertRolePermission() {
        return sqlInsert(RolesPermissionColumns.values());
    }
    // select *
    public String sqlSelectAllRolePermissions() {
        return sqlSelect(false, RolesPermissionColumns.values());
    }
    // select
    public String sqlSelectPermissionsOfRole() {
        return sqlSelectWhere(false, RolesPermissionColumns.ROLE_NAME, RolesPermissionColumns.values());
    }
    // delete target role Permission
    public String sqlDeletePermissionOfRole() {
        return sqlDeleteWhere(RolesPermissionColumns.ROLE_NAME, RolesPermissionColumns.PERMISSION);
    }
    // delete all permission for a role 
    public String sqlDeleteAllPermissionsForRole() {
        return sqlDeleteWhere(RolesPermissionColumns.ROLE_NAME);
    }
    // delete all role permission
    public String sqlDeleteAllRolePermissions() {
        return sqlDeleteAll(RolesPermissionColumns.ROLE_NAME);
    }
    
    // --------------------------------
    // -- TABLE A_ROLES_USERS        --
    // --------------------------------
    
    // tablename
    public String getTableNameRolesUsers() {
        return getTableName(UsersRolesColumns.REF_USER.tableName());
    }
    // create
    public String sqlCreateTableRolesUsers() {
        return sqlCreateTable(UsersRolesColumns.values());
    }
    // insert
    public String sqlInsertRolesUser() {
        return sqlInsert(UsersRolesColumns.values());
    }
    // drop
    public String sqlDropTableRolesUsers() {
        return sqlDropTable(UsersRolesColumns.REF_USER.tableName());
    }
    // select *
    public String sqlSelectAllRolesUsers() {
        return sqlSelect(false, UsersRolesColumns.values());
    }
    
    // TODO
    
    // ---------------------------------
    // -- TABLE Features_Usage        --
    // ---------------------------------

    // tablename
    public String getTableNameFeatureUsage() {
        return getTableName(FeatureUsageColumns.ACTION.tableName());
    }
    // create
    public String sqlCreateTableFeatureUsage() {
        return sqlCreateTable(FeatureUsageColumns.values());
    }
    // drop
    public String sqlDropTableFeatureUsage() {
        return sqlDropTable(FeatureUsageColumns.ACTION.tableName());
    }
    // insert
    public String sqlInsertFeatureUsage() {
        return sqlInsert(FeatureUsageColumns.values());
    }
    // TODO
    
    // ---------------------------------
    // -- TABLE AuditTrail       --
    // ---------------------------------
   
    // tablename
    public String getTableNameAuditTrail() {
        return getTableName(AuditTrailColumns.UID.tableName());
    }
    // create
    public String sqlCreateTableAuditTrail() {
        return sqlCreateTable(AuditTrailColumns.values());
    }
    // drop
    public String sqlDropTableAuditTrail() {
        return sqlDropTable(AuditTrailColumns.ACTION.tableName());
    }
    // insert
    public String sqlInsertAuditTrail() {
        return sqlInsert(AuditTrailColumns.values());
    }
    // delete
    public String sqlDeleteAuditTrail() {
        return sqlDeleteWhere(AuditTrailColumns.UID);
    }
    // select by id
    public String sqlSelectAuditById() {
        return sqlSelectWhere(false, AuditTrailColumns.UID, AuditTrailColumns.values());
    }

    //TODO
    
    
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
}
