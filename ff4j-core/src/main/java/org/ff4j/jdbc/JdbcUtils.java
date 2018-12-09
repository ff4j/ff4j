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

import static org.ff4j.test.AssertUtils.assertHasLength;
import static org.ff4j.test.AssertUtils.assertNotNull;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;

import javax.sql.DataSource;

import org.ff4j.feature.exception.FeatureAccessException;

/**
 * Group utilities methods to work with low-level JDBC.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class JdbcUtils {
    
    private static final JdbcQueryBuilder QUERY_BUILDER =  new JdbcQueryBuilder();
    
    private JdbcUtils() {
    }
    
    /**
     * Any repository may need some security (ACL).
     * 
     * @param ds
     *      current data source
     */
    public static void createSchemaSecurity(DataSource ds) {
        if (!isTableExist(ds, QUERY_BUILDER.getTableNameUser())) {
            executeUpdate(ds, QUERY_BUILDER.sqlCreateTableUser());
        }
        if (!isTableExist(ds, QUERY_BUILDER.getTableNameUserPermissions())) {
            executeUpdate(ds, QUERY_BUILDER.sqlCreateTableUserPermissions());
        }
        if (!isTableExist(ds, QUERY_BUILDER.getTableNameRoles())) {
            executeUpdate(ds, QUERY_BUILDER.sqlCreateTableRoles());
        }
        if (!isTableExist(ds, QUERY_BUILDER.getTableNameRolesPermissions())) {
            executeUpdate(ds, QUERY_BUILDER.sqlCreateTableRolesPermissions());
        }
        if (!isTableExist(ds, QUERY_BUILDER.getTableNameRolesUsers())) {
            executeUpdate(ds, QUERY_BUILDER.sqlCreateTableRolesUsers());
        }
    }
    
    /**
     * Check if target Table exist.
     *
     * @param tableName
     *      table to create
     * @return
     *      if the table exist or not
     */
    public static boolean isTableExist(DataSource ds, String tableName) {
        assertNotNull(ds);
        assertHasLength(tableName);
        Connection sqlConn = null;
        ResultSet rs = null;
        try {
            sqlConn = ds.getConnection();
            DatabaseMetaData dbmd = sqlConn.getMetaData();
            rs = dbmd.getTables(null, null, tableName, new String[] {"TABLE"});
            return rs.next();
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException("Cannot check table existence", sqlEX);
        } finally {
            closeResultSet(rs);
            closeConnection(sqlConn);
        }
    }
    
    /**
     * Create table based on SQL.
     *
     * @param sqlQuery
     *      sql query
     */
    public static void executeUpdate(DataSource ds, String sqlQuery) {
        assertNotNull(ds);
        assertHasLength(sqlQuery);
        Connection sqlConn = null;
        Statement  sqlStmt = null;
        try {
            // Create connection
            sqlConn = ds.getConnection();
            sqlStmt = sqlConn.createStatement();
            sqlStmt.executeUpdate(sqlQuery);
        } catch (SQLException sqlEX) {
            rollback(sqlConn);
            throw new FeatureAccessException("Cannot execute SQL " + sqlQuery, sqlEX);
        } finally {
            closeStatement(sqlStmt);
            closeConnection(sqlConn);
        }
    }
    
    /**
     * Build {@link PreparedStatement} from parameters
     * 
     * @param query
     *            query template
     * @param params
     *            current parameters
     * @return working {@link PreparedStatement}
     * @throws SQLException
     *             sql error when working with statement
     */
    public static PreparedStatement buildStatement(Connection sqlConn, String query, Object... params)
    throws SQLException {
        PreparedStatement ps = sqlConn.prepareStatement(query);
        if (params != null && params.length > 0) {
            for (int i = 0; i < params.length; i++) {
                //ps.setString(i + 1, params[i]);
                ps.setObject(i + 1, params[i]);
            }
        }
        return ps;
    }

    /**
     * Close resultset.
     * 
     * @param rs
     *            target resultset
     */
    public static void closeResultSet(ResultSet rs) {
        try {
            if (rs != null) {
                rs.close();
            }
        } catch (SQLException e) {
            throw new FeatureAccessException("An error occur when closing resultset", e);
        }
    }
    
    /**
     * Utility method to close statement properly.
     * 
     * @param ps
     * 
     */
    public static void closeStatement(PreparedStatement ps) {
        try {
            if (ps != null) {
                ps.close();
            }
        } catch (SQLException e) {
            throw new FeatureAccessException("An error occur when closing statement", e);
        }
    }
    

    /**
     * Utility method to close statement properly.
     * 
     * @param ps
     * 
     */
    public static void closeStatement(Statement ps) {
        try {
            if (ps != null) {
                ps.close();
            }
        } catch (SQLException e) {
            throw new FeatureAccessException("An error occur when closing statement", e);
        }
    }
    
    /**
     * Return connection to pool.
     *
     * @param sqlConnection
     */
    public static void closeConnection(Connection sqlConnection) {
        try {
            if (sqlConnection != null && !sqlConnection.isClosed()) {
                sqlConnection.close();
            }
        } catch (SQLException e) {
            throw new FeatureAccessException("An error occur when closing statement", e);
        }
    }
    
    /**
     * Utility method to perform rollback in correct way.
     * 
     * @param sqlConn
     *            current sql connection
     */
    public static void rollback(Connection sqlConn) {
        try {
            if (sqlConn != null && !sqlConn.isClosed()) {
                sqlConn.rollback();
            }
        } catch (SQLException e) {
            throw new FeatureAccessException("Cannot rollback database, SQL ERROR", e);
        }
    }
}
