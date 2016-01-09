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
import java.sql.SQLException;
import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.sql.DataSource;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventType;
import org.ff4j.audit.graph.BarChart;
import org.ff4j.audit.graph.BarSeries;
import org.ff4j.audit.graph.PieChart;
import org.ff4j.audit.graph.PieSector;
import org.ff4j.exception.AuditAccessException;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.store.JdbcStoreConstants;
import org.ff4j.utils.Util;

/**
 * Implementation of in memory {@link EventRepository} with limited events.
 * 
 * @author Cedrick Lunven (@clunven)
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
        int totalEvent = 0;
        try {
            // Get collection from Pool
            sqlConn = dataSource.getConnection();
            stmt = sqlConn.prepareStatement(SQL_AUDIT_COUNT);
            rs = stmt.executeQuery();
            // Should be ok
            rs.next();
            totalEvent =  rs.getInt(1);
        } catch(Exception exc) {
            throw new AuditAccessException("Cannot read audit information from database ", exc);
        } finally {
           closeResultSet(rs);
           closeStatement(stmt);
           closeConnection(sqlConn);
        }
        return totalEvent;
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
            throw new AuditAccessException("Cannot insert event into DB", exc);
            
        } finally {
           closeStatement(stmt);
           closeConnection(sqlConn);
        }
        return true;
    }

    /** {@inheritDoc} */
    @Override
    public Set < String > getFeatureNames() {
        Set < String> listOfFeatureNames = new HashSet<String>();
        Connection sqlConn = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            // Returns features
            sqlConn = dataSource.getConnection();
            ps = sqlConn.prepareStatement(SQL_AUDIT_LISTFEATURES);
            rs = ps.executeQuery();
            while (rs.next()) {
                listOfFeatureNames.add(rs.getString(COL_EVENT_UID));
            }
            return listOfFeatureNames;
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException("Cannot check feature existence, error related to database", sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
            closeConnection(sqlConn);
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public PieChart getHitsPieChart(long startTime, long endTime) {
        PieChart pieGraph = new PieChart(TITLE_PIE_HITCOUNT);
        Connection sqlConn = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            // Returns features
            sqlConn = dataSource.getConnection();
            int idx = 0;
            Set < String > features = getFeatureNames();
            List < String > colors  = Util.getColorsGradient(features.size());
            for (String featName : features) {
                int counter = 0;
                ps = sqlConn.prepareStatement(SQL_AUDIT_COUNTFEATURE);
                ps.setString(1, featName);
                ps.setTimestamp(2, new Timestamp(startTime));
                ps.setTimestamp(3, new Timestamp(endTime));
                rs = ps.executeQuery();
                rs.next();
                counter = rs.getInt(1);
                pieGraph.getSectors().add(new PieSector(featName, counter, colors.get(idx)));
            }
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException("Cannot build PieChart from repository, ", sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
            closeConnection(sqlConn);
        }
       
        return pieGraph;
    }
    
    /** {@inheritDoc} */
    @Override
    public BarChart getHitsBarChart(Set<String> featNameSet, long startTime, long endTime, int nbslot) {
        
        // Build Labels
        long slotWitdh = (endTime - startTime) / nbslot;
        SimpleDateFormat sdf = new SimpleDateFormat("HH:mm");
        List <String> labels = new ArrayList<String>();
        for (int i = 0; i < nbslot; i++) {
            labels.add(sdf.format(new Date(startTime + slotWitdh * i)));
        }
        
        // Build SeriesNames
        BarChart barChart = new BarChart(TITLE_BARCHAR_HIT, labels, new ArrayList<String>(featNameSet));
        
        Connection sqlConn = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            // Returns features
            sqlConn = dataSource.getConnection();
            
            for (String featName : getFeatureNames()) {
                ps = sqlConn.prepareStatement(SQL_AUDIT_FEATURE_EVENTOK);
                ps.setString(1, featName);
                ps.setTimestamp(2, new Timestamp(startTime));
                ps.setTimestamp(3, new Timestamp(endTime));
                rs = ps.executeQuery();
                
                BarSeries currentSeries = barChart.getSeries().get(featName);
                while (rs.next()) {
                    long timestamp   = rs.getTimestamp(COL_EVENT_TIME).getTime();
                    currentSeries.incrCount((int) ((timestamp - startTime) / slotWitdh));
                }
            }
                
        } catch (SQLException sqlEX) {
            throw new AuditAccessException("Cannot build PieChart from repository, ", sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
            closeConnection(sqlConn);
        }
        return barChart;
    }

    /** {@inheritDoc} */
    @Override
    public PieChart getFeatureHitsPie(String featureId, long startTime, long endTime) {
        List < String > colors   = Util.getColorsGradient(4);
        PieChart pieGraph = new PieChart("Hits Count for " + featureId);
        
        Connection sqlConn = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            // Returns features
            sqlConn = dataSource.getConnection();
            ps = sqlConn.prepareStatement(SQL_AUDIT_FEATURE_ALLEVENTS);
            ps.setString(1, featureId);
            ps.setTimestamp(2, new Timestamp(startTime));
            ps.setTimestamp(3, new Timestamp(endTime));
            rs = ps.executeQuery();
            
            int nbEnable = 0;
            int nbDisable = 0;
            int nbFlip = 0;
            int notFlip = 0;
            while (rs.next()) {
                String eventTypeString = rs.getString(COL_EVENT_TYPE);
                EventType eventType = EventType.valueOf(eventTypeString);
                switch (eventType) {
                    case FEATURE_CHECK_ON:
                        nbFlip++;
                    break;
                    case FEATURE_CHECK_OFF:
                        notFlip++;
                    break;
                    case ENABLE_FEATURE:
                        nbEnable++;
                    break;
                    case DISABLE_FEATURE:
                        nbDisable++;
                    default:
                    break;
                }
            }
            if (nbEnable > 0) {
                pieGraph.getSectors().add(
                        new PieSector(EventType.ENABLE_FEATURE.toString(), nbEnable, colors.get(0)));
            }
            if (nbDisable > 0) {
                pieGraph.getSectors().add(new PieSector(
                        EventType.DISABLE_FEATURE.toString(), nbDisable, colors.get(1)));
            }
            if (nbFlip > 0) {
                pieGraph.getSectors().add(new PieSector(
                        EventType.FEATURE_CHECK_ON.toString(), nbFlip, colors.get(2)));
            }
            if (notFlip > 0) {
                pieGraph.getSectors().add(new PieSector(
                        EventType.FEATURE_CHECK_OFF.toString(), notFlip, colors.get(3)));
            }
            return pieGraph;
                
        } catch (SQLException sqlEX) {
            throw new AuditAccessException("Cannot build PieChart from repository, ", sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
            closeConnection(sqlConn);
        }
        
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
