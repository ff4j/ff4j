package org.ff4j.test.utils;

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
import java.sql.Connection;

import javax.sql.DataSource;

import org.apache.commons.dbcp.BasicDataSource;
import org.ff4j.utils.JdbcUtils;

/**
 * Mutualize some JDBC operation (DataSource initialization for instance)
 * Class to TODO
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 *
 */
public class JdbcTestHelper {
    
    /**
     * Hider public constructor
     */
    private JdbcTestHelper() {
    }
    
    /**
     * Initialize DataSource with a pool of connections to HQL database.
     *
     * @return
     *      current data source
     */
    public static DataSource createInMemoryHQLDataSource() {
        // Init DataSource
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
    public static void initDBSchema(DataSource dataSource, boolean dropTable) {
        Connection sqlConnection = null;
        try {
            sqlConnection = dataSource.getConnection();
            SqlScriptRunner ssr = new SqlScriptRunner(sqlConnection, true, true);
            if (dropTable) {
                ssr.runScript(new FileReader("src/main/resources/schema-drop.sql"));
            }
            ssr.runScript(new FileReader("src/main/resources/schema-ddl.sql"));
            ssr.runScript(new FileReader("src/test/resources/ff-store.sql"));
            
        } catch (Exception e) {
            throw new IllegalArgumentException("Cannot initialized DB", e);
            
        } finally {
            JdbcUtils.closeConnection(sqlConnection);
        }
    }
    
    

}
