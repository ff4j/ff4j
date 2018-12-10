package org.ff4j.jdbc;

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


import java.io.FileReader;
import java.io.StringReader;
import java.sql.Connection;

import javax.sql.DataSource;

import org.apache.commons.dbcp.BasicDataSource;
import org.ff4j.jdbc.JdbcUtils;


/**
 * Test JDBC ELEMENTS.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class JdbcTestHelper {
    
    /**
     * Hider public constructor
     */
    private JdbcTestHelper() {}
    
    /**
     * Initialize DataSource with a pool of connections to HQL database.
     *
     * @return
     *      current data source
     */
    public static DataSource createInMemoryHQLDataSource() {
        BasicDataSource dbcpDataSource = new BasicDataSource();
        dbcpDataSource.setDriverClassName("org.hsqldb.jdbcDriver");
        dbcpDataSource.setUsername("sa");
        dbcpDataSource.setPassword("");
        dbcpDataSource.setUrl("jdbc:hsqldb:mem:.");
        dbcpDataSource.setMaxActive(3);
        dbcpDataSource.setMaxIdle(2);
        dbcpDataSource.setInitialSize(2);
        dbcpDataSource.setValidationQuery("select 1 from INFORMATION_SCHEMA.SYSTEM_USERS;");
        dbcpDataSource.setPoolPreparedStatements(true);
        return dbcpDataSource;
    }
    
    /**
     * Execute expected SQL scripts.
     */
    public static void initDBSchema(DataSource dataSource) {
        Connection sqlConnection = null;
        try {
            sqlConnection = dataSource.getConnection();
            SqlScriptRunner ssr = new SqlScriptRunner(sqlConnection, true, true);
            
            try {
                ssr.runScript(new StringReader(new JdbcQueryBuilder().sqlDropSchema()));
            } catch(Exception e) { 
                // if table does not exist you may failed, not a big deal
            }
            ssr.runScript(new StringReader(new JdbcQueryBuilder().sqlCreateSchema()));
            ssr.runScript(new FileReader("src/test/resources/ff4j-testDataset.sql"));
            
        } catch (Exception e) {
            throw new IllegalArgumentException("Cannot initialized DB", e);
            
        } finally {
            JdbcUtils.closeConnection(sqlConnection);
        }
    }

}
