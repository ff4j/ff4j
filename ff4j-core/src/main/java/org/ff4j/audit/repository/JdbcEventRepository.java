package org.ff4j.audit.repository;

import static org.ff4j.audit.EventConstants.TITLE_BARCHAR_HIT;
import static org.ff4j.audit.EventConstants.TITLE_PIE_HITCOUNT;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_ACTION;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_NAME;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_TIME;

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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.sql.DataSource;

import org.ff4j.audit.Event;
import org.ff4j.audit.graph.BarChart;
import org.ff4j.audit.graph.BarSeries;
import org.ff4j.audit.graph.PieChart;
import org.ff4j.audit.graph.PieSector;
import org.ff4j.exception.AuditAccessException;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.store.JdbcQueryBuilder;
import org.ff4j.utils.Util;

/**
 * Implementation of in memory {@link EventRepository} with limited events.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class JdbcEventRepository extends AbstractEventRepository {

	/** error message. */
    public static final String CANNOT_BUILD_PIE_CHART_FROM_REPOSITORY = 
    		"Cannot build PieChart from repository, ";
    
    /** Access to storage. */
    private DataSource dataSource;
    
    /** Query builder. */
    private JdbcQueryBuilder queryBuilder;

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
    public int getTotalEventCount() {
        Connection        sqlConn = null;
        PreparedStatement stmt = null;
        ResultSet         rs = null;
        int totalEvent = 0;
        try {
            // Get collection from Pool
            sqlConn = dataSource.getConnection();
            stmt = sqlConn.prepareStatement(getQueryBuilder().countAudit());
            rs = stmt.executeQuery();
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
    public boolean saveEvent(Event evt) {
        Util.assertNotNull(evt);
        Util.assertHasLength(evt.getName());
        Util.assertHasLength(evt.getType());
        Util.assertHasLength(evt.getAction());
        
        Connection        sqlConn = null;
        PreparedStatement stmt = null;
        try {
            // Get collection from Pool
            sqlConn = dataSource.getConnection();
           
            // Open TX Bloc
            sqlConn.setAutoCommit(false);
            int idx = 9;
            Map < Integer, String > statementParams = new HashMap<Integer, String>();
            
            StringBuilder sb = new StringBuilder("INSERT INTO " +
            		getQueryBuilder().getTableName("AUDIT") + 
            		"(EVT_UUID,EVT_TIME,EVT_TYPE,EVT_NAME,EVT_ACTION,EVT_HOSTNAME,EVT_SOURCE,EVT_DURATION");
            if (Util.hasLength(evt.getUser())) {
                sb.append(", EVT_USER");
                statementParams.put(idx, evt.getUser());
                idx++;
            }
            if (Util.hasLength(evt.getValue())) {
                sb.append(", EVT_VALUE");
                statementParams.put(idx, evt.getValue());
                idx++;
            }
            if (!evt.getCustomKeys().isEmpty()) {
                sb.append(", EVT_KEYS");
                statementParams.put(idx, evt.getCustomKeys().toString());
                idx++;
            }
            
            sb.append(") VALUES (?");
            for(int offset = 1; offset < idx-1;offset++) {
                sb.append(",?");
            }
            sb.append(")");
            
            stmt = sqlConn.prepareStatement(sb.toString());
            stmt.setString(1, evt.getUuid());
            stmt.setTimestamp(2, new java.sql.Timestamp(evt.getTimestamp()));
            stmt.setString(3, evt.getType());
            stmt.setString(4, evt.getName());
            stmt.setString(5, evt.getAction());
            stmt.setString(6, evt.getHostName());
            stmt.setString(7, evt.getSource());
            stmt.setLong(8, evt.getDuration());
            for (int id = 9;id < idx;id++) {
                stmt.setString(id, statementParams.get(id));
            }
            
            // Execute Query
            stmt.executeUpdate();
            
            // Commit TX
            sqlConn.commit();
            
        } catch(Exception exc) {
            throw new AuditAccessException("Cannot insert event into DB (" + exc.getClass() + ") "+ exc.getCause(), exc);
            
        } finally {
           closeStatement(stmt);
           closeConnection(sqlConn);
        }
        return true;
    }

    /** {@inheritDoc} */
    public Set < String > getFeatureNames() {
        Set < String> listOfFeatureNames = new HashSet<String>();
        Connection sqlConn = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            // Returns features
            sqlConn = dataSource.getConnection();
            ps = sqlConn.prepareStatement(getQueryBuilder().listFeaturesAudit());
            rs = ps.executeQuery();
            while (rs.next()) {
                listOfFeatureNames.add(rs.getString(COL_EVENT_NAME));
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
    public PieChart featuresListDistributionPie(long startTime, long endTime) {
        PieChart pieGraph = new PieChart(TITLE_PIE_HITCOUNT);
        Connection sqlConn = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            // Returns features
            sqlConn = dataSource.getConnection();
            
            
            ps = sqlConn.prepareStatement(getQueryBuilder().getFeaturesPieAudit());
            ps.setTimestamp(1, new Timestamp(startTime));
            ps.setTimestamp(2, new Timestamp(endTime));
            rs = ps.executeQuery();
            Map < String, Integer > freq = new HashMap<String, Integer>();
            while (rs.next()) {
                freq.put(rs.getString(COL_EVENT_NAME), rs.getInt("NB"));
            }
            List < String > colors  = Util.getColorsGradient(freq.size());
            int idx = 0;
            for (String featName : freq.keySet()) {
                pieGraph.getSectors().add(
                        new PieSector(featName, freq.get(featName), colors.get(idx)));
                idx++;
            }
            
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException(CANNOT_BUILD_PIE_CHART_FROM_REPOSITORY, sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
            closeConnection(sqlConn);
        }
       
        return pieGraph;
    }
    
    /** {@inheritDoc} */
    public BarChart getFeaturesUsageOverTime(Set<String> featNameSet, long startTime, long endTime, int nbslot) {
        
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
                ps = sqlConn.prepareStatement(getQueryBuilder().getAllEventsFeatureAudit());
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
            throw new AuditAccessException(CANNOT_BUILD_PIE_CHART_FROM_REPOSITORY, sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
            closeConnection(sqlConn);
        }
        return barChart;
    }

    /** {@inheritDoc} */
    public PieChart featureDistributionPie(String featureId, long startTime, long endTime) {
        PieChart pieGraph = new PieChart("Hits Count for " + featureId);
        
        Connection sqlConn = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            // Returns features
            sqlConn = dataSource.getConnection();
            ps = sqlConn.prepareStatement(getQueryBuilder().getFeatureDistributionAudit());
            ps.setString(1, featureId);
            ps.setTimestamp(2, new Timestamp(startTime));
            ps.setTimestamp(3, new Timestamp(endTime));
            rs = ps.executeQuery();
            
            Map < String, Integer > freq = new HashMap<String, Integer>();
            while (rs.next()) {
                freq.put(rs.getString(COL_EVENT_ACTION), rs.getInt("NB"));
            }
            List < String > colors = Util.getColorsGradient(freq.size());
            int idx = 0;
            for (String action : freq.keySet()) {
                pieGraph.getSectors().add(new PieSector(action, freq.get(action), colors.get(idx)));
                idx++;
            }
            return pieGraph;
                
        } catch (SQLException sqlEX) {
            throw new AuditAccessException(CANNOT_BUILD_PIE_CHART_FROM_REPOSITORY, sqlEX);
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
    
    /**
	 * @return the queryBuilder
	 */
	public JdbcQueryBuilder getQueryBuilder() {
		if (queryBuilder == null) {
			queryBuilder = new JdbcQueryBuilder();
		}
		return queryBuilder;
	}

	/**
	 * @param queryBuilder the queryBuilder to set
	 */
	public void setQueryBuilder(JdbcQueryBuilder queryBuilder) {
		this.queryBuilder = queryBuilder;
	}
}
