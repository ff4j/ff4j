package org.ff4j.audit.repository;

/*
 * #%L ff4j-core %% Copyright (C) 2013 - 2015 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import static org.ff4j.utils.JdbcUtils.closeConnection;
import static org.ff4j.utils.JdbcUtils.closeResultSet;
import static org.ff4j.utils.JdbcUtils.closeStatement;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.Set;

import javax.sql.DataSource;

import org.ff4j.audit.Event;
import org.ff4j.audit.graph.BarChart;
import org.ff4j.audit.graph.PieChart;
import org.ff4j.store.JdbcStoreConstants;
import org.ff4j.utils.Util;

/**
 * Implementation of in memory {@link EventRepository} with limited events.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class JdbcEventRepository extends AbstractEventRepository implements JdbcStoreConstants {

    /** Access to storage. */
    private DataSource dataSource;

    /**
     * Constructor from DataSource.
     * 
     * @param jdbcDS
     *            native jdbc datasource
     */
    public JdbcEventRepository(DataSource jdbcDS) {
        this.dataSource = jdbcDS;
    }
    
    /** {@inheritDoc} */
    @Override
    public int getTotalEventCount() {
        Connection        sqlConn = null;
        PreparedStatement stmt = null;
        ResultSet         rs = null;
        try {
            // Get collection from Pool
            sqlConn = dataSource.getConnection();
            stmt = sqlConn.prepareStatement(SQL_AUDIT_COUNT);
            rs = stmt.executeQuery();
            if (rs.next()) {
                return rs.getInt(1);
            }
         
        } catch(Exception exc) {
            throw new RuntimeException("Cannot insert event into DB", exc);
            
        } finally {
           closeResultSet(rs);
           closeStatement(stmt);
           closeConnection(sqlConn);
        }
        return 0;
    }

    /** {@inheritDoc} */
    @Override
    public boolean saveEvent(Event evt) {
        Util.assertNotNull(evt);
        Util.assertHasLength(evt.getFeatureName());
        
        Connection        sqlConn = null;
        PreparedStatement stmt = null;
        try {
            // Get collection from Pool
            sqlConn = dataSource.getConnection();
           
            // Open TX Bloc
            sqlConn.setAutoCommit(false);
            stmt = sqlConn.prepareStatement(SQL_AUDIT_INSERT);
            stmt.setTimestamp(1, new java.sql.Timestamp(evt.getTimestamp()));
            stmt.setString(2, evt.getType().toString());
            stmt.setString(3,  evt.getFeatureName());
            
            // Execute Query
            stmt.executeUpdate();
            
            // Commit TX
            sqlConn.commit();
            
        } catch(Exception exc) {
            throw new RuntimeException("Cannot insert event into DB", exc);
            
        } finally {
           closeStatement(stmt);
           closeConnection(sqlConn);
        }
        return false;
    }

    /** {@inheritDoc} */
    @Override
    public PieChart getHitsPieChart(long startTime, long endTime) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public BarChart getHitsBarChart(Set<String> featNameSet, long startTime, long endTime, int nbslot) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public BarChart getHitsBarChart(long startTime, long endTime, int nbslot) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public PieChart getFeatureHitsPie(String featureId, long startTime, long endTime) {
        // TODO Auto-generated method stub
        return null;
    }

    

    /**
     * Getter accessor for attribute 'dataSource'.
     *
     * @return current value of 'dataSource'
     */
    public DataSource getDataSource() {
        return dataSource;
    }

    /**
     * Setter accessor for attribute 'dataSource'.
     * 
     * @param dataSource
     *            new value for 'dataSource '
     */
    public void setDataSource(DataSource dataSource) {
        this.dataSource = dataSource;
    }
}
