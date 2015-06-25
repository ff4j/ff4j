package org.ff4j.test.store;

/*
 * #%L ff4j-core %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License"); you may not use
 * this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import java.io.FileReader;
import java.sql.Connection;
import java.sql.SQLException;

import javax.sql.DataSource;

import org.apache.commons.dbcp.BasicDataSource;
import org.ff4j.core.FeatureStore;
import org.ff4j.store.JdbcFeatureStore;
import org.ff4j.test.utils.SqlScriptRunner;
import org.junit.After;
import org.junit.Before;

/**
 * This test is meant to test the JDBC feature store directly.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class JdbcFeatureDataSourceTest extends AbstractStoreTest {

    /** SQL DataSource. */
    private DataSource sqlDataSource;
    
    /** Should reinit tables on each test , except first */
    private static boolean tableCreated = false;
  
    /** {@inheritDoc} */
    @Override
    protected FeatureStore initStore() {
    	sqlDataSource = initHQLInMemoryDataSource();
        return new JdbcFeatureStore(sqlDataSource);
    }
    
    /**
     * Initialize DataSource with a pool of connections to HQL database.
     *
     * @return
     * 		current data source
     */
    private DataSource initHQLInMemoryDataSource() {
    	// Init DataSource
    	BasicDataSource dbcpDataSource = new BasicDataSource();
    	dbcpDataSource.setDriverClassName("org.hsqldb.jdbcDriver");
    	dbcpDataSource.setUsername("sa");
    	dbcpDataSource.setPassword("");
    	dbcpDataSource.setUrl("jdbc:hsqldb:mem:.");
    	dbcpDataSource.setMaxActive(100);
    	dbcpDataSource.setMaxIdle(50);
    	dbcpDataSource.setInitialSize(50);
    	dbcpDataSource.setValidationQuery("select 1 from INFORMATION_SCHEMA.SYSTEM_USERS;");
    	dbcpDataSource.setPoolPreparedStatements(true);
    	return dbcpDataSource;
    }
    
    /**
     * Execute expected SQL scripts.
     */
    private void runDDL() {
    	// Create a SQL Connection and execute DDL
    	SqlScriptRunner ssr;
    	Connection sqlConnection = null;
		try {
			sqlConnection = sqlDataSource.getConnection();
			ssr = new SqlScriptRunner(sqlConnection, true, true);
			if (tableCreated) {
				ssr.runScript(new FileReader("src/main/resources/schema-drop.sql"));
			}
			ssr.runScript(new FileReader("src/main/resources/schema-ddl.sql"));
			ssr.runScript(new FileReader("src/test/resources/ff-store.sql"));
			tableCreated = true;
		} catch (Exception e) {
			throw new IllegalArgumentException("Cannot initialized DB", e);
		} finally {
			try {
				sqlConnection.commit();
				sqlConnection.close();
			} catch (SQLException e) {}
		}
    }

    /** {@inheritDoc} */
    @Override
    @Before
    public void setUp() throws Exception {
        super.setUp();
        runDDL();
    }

    /** {@inheritDoc} */
    @After
    public void tearDown() throws Exception {
    }
}
