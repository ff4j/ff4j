package org.ff4j.utils;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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


import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import org.ff4j.exception.FeatureAccessException;

/**
 * Group utilities methods to work with low-level JDBC.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class JdbcUtils {
    
    private JdbcUtils() {
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
    public static PreparedStatement buildStatement(Connection sqlConn, String query, String... params)
    throws SQLException {
        PreparedStatement ps = sqlConn.prepareStatement(query);
        if (params != null && params.length > 0) {
            for (int i = 0; i < params.length; i++) {
                ps.setString(i + 1, params[i]);
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
