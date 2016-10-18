package org.ff4j.test.store;

import static org.ff4j.utils.JdbcUtils.closeConnection;
import static org.ff4j.utils.JdbcUtils.closeResultSet;
import static org.ff4j.utils.JdbcUtils.closeStatement;
import static org.mockito.Mockito.doThrow;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

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

import javax.sql.DataSource;

import org.ff4j.core.FeatureStore;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.store.JdbcFeatureStore;
import org.ff4j.test.utils.JdbcTestHelper;
import org.ff4j.utils.JdbcUtils;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.Mockito;

import static org.ff4j.store.JdbcStoreConstants.*;

/**
 * This test is meant to access a Jfeature store in 'pure' JDBC.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class JdbcFeatureDataSourceTest extends CoreFeatureStoreTestSupport {

    /** SQL DataSource. */
    private DataSource sqlDataSource;
    
    /** Should reinit tables on each test , except first */
    private static boolean dropTable = false;
  
    /** {@inheritDoc} */
    @Override
    protected FeatureStore initStore() {
    	sqlDataSource = JdbcTestHelper.createInMemoryHQLDataSource();
        return new JdbcFeatureStore(sqlDataSource);
    }
    
    /** {@inheritDoc} */
    @Override
    @Before
    public void setUp() throws Exception {
        super.setUp();
        JdbcTestHelper.initDBSchema(sqlDataSource, dropTable);
        dropTable = true;
    }
    
    @Test(expected = FeatureAccessException.class)
    public void testClose2Times() {
        Connection          sqlConn = null;
        PreparedStatement   ps = null;
        try {
            // Pick connection
            sqlConn = sqlDataSource.getConnection();
            
            // Query Exist
            ps = JdbcUtils.buildStatement(sqlConn, SQL_EXIST, F1);
            JdbcUtils.rollback(sqlConn);
            ps.close();
            ps.executeQuery();
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException("Cannot check feature existence, error related to database", sqlEX);
        } 
    }

    
    @Test
    public void testClosess() {
        Connection          sqlConn = null;
        PreparedStatement   ps = null;
        ResultSet           rs = null;
        try {
            
            // Pick connection
            sqlConn = sqlDataSource.getConnection();
            
            // Query Exist
            ps = JdbcUtils.buildStatement(sqlConn, SQL_EXIST, F1);
            JdbcUtils.rollback(sqlConn);
            ps.close();
            //rs = ps.executeQuery();
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException("Cannot check feature existence, error related to database", sqlEX);
        } finally {
            closeResultSet(rs);
            closeResultSet(rs);
            closeResultSet(null);
            closeStatement(ps);
            closeStatement(ps);
            closeStatement(null);
            closeConnection(sqlConn);
            closeConnection(sqlConn);
            closeConnection(null);
        }
    }
    
    @Test(expected = FeatureAccessException.class)
    public void testClosePS() throws SQLException {
        PreparedStatement test = Mockito.mock(PreparedStatement.class);
        doThrow(new SQLException()).when(test).close();
        closeStatement(test);
    }
    
    @Test(expected = FeatureAccessException.class)
    public void testCloseRS() throws SQLException {
        ResultSet test = Mockito.mock(ResultSet.class);
        doThrow(new SQLException()).when(test).close();
        closeResultSet(test);
    }
    
    @Test(expected = FeatureAccessException.class)
    public void testCloseConn() throws SQLException {
        Connection test = Mockito.mock(Connection.class);
        doThrow(new SQLException()).when(test).close();
        closeConnection(test);
    }
    
    @Test(expected = FeatureAccessException.class)
    public void testRollback() throws SQLException {
        Connection test = Mockito.mock(Connection.class);
        doThrow(new SQLException()).when(test).rollback();
        JdbcUtils.rollback(test);
    }
   
    @Test
    public void testDS() throws SQLException {
        DataSource test = Mockito.mock(DataSource.class);
        doThrow(new SQLException()).when(test).getConnection();
        Assert.assertFalse(ff4j.getFeatureStore().exist("I-DONT-EXIST"));
    }

}
