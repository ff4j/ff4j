package org.ff4j.audit.repository;


import static org.ff4j.audit.EventConstants.ACTION_CHECK_OK;
import static org.ff4j.store.JdbcStoreConstants.*;

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
import static org.ff4j.utils.JdbcUtils.executeUpdate;
import static org.ff4j.utils.JdbcUtils.isTableExist;
import static org.ff4j.utils.JdbcUtils.rollback;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import javax.sql.DataSource;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.EventSeries;
import org.ff4j.audit.MutableHitCount;
import org.ff4j.audit.chart.TimeSeriesChart;
import org.ff4j.exception.AuditAccessException;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.store.JdbcEventMapper;
import org.ff4j.store.JdbcQueryBuilder;
import org.ff4j.utils.MappingUtil;
import org.ff4j.utils.Util;

/**
 * Implementation of in memory {@link EventRepository} with limited events.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class JdbcEventRepository extends AbstractEventRepository {
    
    /** Error message 1. */
    public static final String CANNOT_READ_AUDITTABLE =  "Cannot read audit table from DB";

	/** error message. */
    public static final String CANNOT_BUILD_PIE_CHART_FROM_REPOSITORY = "Cannot build PieChart from repository, ";
    
    /** Access to storage. */
    private DataSource dataSource;
    
    /** Query builder. */
    private JdbcQueryBuilder queryBuilder;
    
    /** Mapper to read from SQL result. */
    private static final JdbcEventMapper EVENT_MAPPER = new JdbcEventMapper();

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
    public void createSchema() {
        DataSource       ds = getDataSource();
        JdbcQueryBuilder qb = getQueryBuilder();
        if (!isTableExist(ds, qb.getTableNameAudit())) {
            executeUpdate(ds, qb.sqlCreateTableAudit());
        }
    }
    
    /** {@inheritDoc} */
    // FIXME Stop dynamic query !
    @Override
    public boolean saveEvent(Event evt) {
        Util.assertEvent(evt);
        
        Connection        sqlConn = null;
        PreparedStatement stmt = null;
        try {
            // Get collection from Pool
            sqlConn = dataSource.getConnection();
           
            // Open TX Bloc
            sqlConn.setAutoCommit(false);
            int idx = 9;
            Map < Integer, String > statementParams = new HashMap<Integer, String>();
            
            StringBuilder sb = new StringBuilder("INSERT INTO " + getQueryBuilder().getTableNameAudit() + 
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
                statementParams.put(idx, MappingUtil.fromMap(evt.getCustomKeys()));
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
            rollback(sqlConn);
            throw new AuditAccessException("Cannot insert event into DB (" + exc.getClass() + ") "+ exc.getCause(), exc);
        } finally {
           closeStatement(stmt);
           closeConnection(sqlConn);
        }
        return true;
    }
    
    /** {@inheritDoc} */
    @Override
    public Event getEventByUUID(String uuid, Long timestamp) {
        Util.assertHasLength(uuid);
        Connection          sqlConn = null;
        PreparedStatement   ps = null;
        ResultSet           rs = null;
        try {
            sqlConn = getDataSource().getConnection();
            ps = sqlConn.prepareStatement(getQueryBuilder().getEventByUuidQuery());
            ps.setString(1, uuid);
            rs = ps.executeQuery();
            if (rs.next()) {
               return EVENT_MAPPER.mapEvent(rs);
            }
            return null;
        } catch (SQLException sqlEX) {
            throw new IllegalStateException("CANNOT_READ_AUDITTABLE", sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
            closeConnection(sqlConn);
        }
    }
     
    /** {@inheritDoc} */
    @Override
    public void purgeAuditTrail(EventQueryDefinition qDef) {
        Util.assertNotNull(qDef);
        Connection          sqlConn = null;
        PreparedStatement   ps = null;
        ResultSet           rs = null;
        try {
           sqlConn = getDataSource().getConnection();
           ps = sqlConn.prepareStatement(getQueryBuilder().getPurgeAuditTrailQuery(qDef));
           ps.setTimestamp(1, new java.sql.Timestamp(qDef.getFrom()));
           ps.setTimestamp(2, new java.sql.Timestamp(qDef.getTo()));
           ps.executeUpdate();
        } catch (SQLException sqlEX) {
            throw new IllegalStateException("CANNOT_READ_AUDITTABLE", sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
            closeConnection(sqlConn);
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public void purgeFeatureUsage(EventQueryDefinition qDef) {
        Util.assertNotNull(qDef);
        // Enforce remove "checks"
        qDef.getActionFilters().add(ACTION_CHECK_OK);
        
        Connection          sqlConn = null;
        PreparedStatement   ps = null;
        ResultSet           rs = null;
        try {
           sqlConn = getDataSource().getConnection();
            ps = sqlConn.prepareStatement(getQueryBuilder().getPurgeFeatureUsageQuery(qDef));
            ps.setTimestamp(1, new java.sql.Timestamp(qDef.getFrom()));
            ps.setTimestamp(2, new java.sql.Timestamp(qDef.getTo()));
            ps.executeUpdate();
        } catch (SQLException sqlEX) {
            throw new IllegalStateException("CANNOT_READ_AUDITTABLE", sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
            closeConnection(sqlConn);
        }
    }
    
    /** {@inheritDoc} */
    private EventSeries searchEvents(String sqlQuery, long from, long to) {
        Connection          sqlConn = null;
        PreparedStatement   ps = null;
        ResultSet           rs = null;
        EventSeries         es = new EventSeries();
        try {
            sqlConn = getDataSource().getConnection();
            ps = sqlConn.prepareStatement(sqlQuery);
            ps.setTimestamp(1, new Timestamp(from));
            ps.setTimestamp(2, new Timestamp(to));
            rs = ps.executeQuery();
            while (rs.next()) {
                es.add(EVENT_MAPPER.mapEvent(rs));
            }
        } catch (SQLException sqlEX) {
            throw new IllegalStateException("CANNOT_READ_AUDITTABLE", sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
            closeConnection(sqlConn);
        }
        return es;
    }
    
    /** {@inheritDoc} */
    private  Map<String, MutableHitCount> computeHitCount(String sqlQuery, String columnName, long from, long to) {
        Connection          sqlConn = null;
        PreparedStatement   ps = null;
        ResultSet           rs = null;
        Map<String, MutableHitCount>  hitCount = new HashMap<String, MutableHitCount>();
        try {
            // Returns features
            sqlConn = dataSource.getConnection();
            ps = sqlConn.prepareStatement(sqlQuery);
            ps.setTimestamp(1, new Timestamp(from));
            ps.setTimestamp(2, new Timestamp(to));
            rs = ps.executeQuery();
            while (rs.next()) {
                hitCount.put(rs.getString(columnName), new MutableHitCount(rs.getInt("NB")));
            } 
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException(CANNOT_BUILD_PIE_CHART_FROM_REPOSITORY, sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
            closeConnection(sqlConn);
        }
        return hitCount;
    }
    
    /** {@inheritDoc} */
    @Override
    public EventSeries getAuditTrail(EventQueryDefinition qDef) {
        return searchEvents(getQueryBuilder().getSelectAuditTrailQuery(qDef), qDef.getFrom(), qDef.getTo());
    }

    /** {@inheritDoc} */
    @Override
    public EventSeries searchFeatureUsageEvents(EventQueryDefinition qDef) {
        return searchEvents(getQueryBuilder().getSelectFeatureUsageQuery(qDef), qDef.getFrom(), qDef.getTo());
    }
        
    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getFeatureUsageHitCount(EventQueryDefinition query) {
        return computeHitCount(getQueryBuilder().getFeaturesHitCount(), COL_EVENT_NAME, query.getFrom(), query.getTo());
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getHostHitCount(EventQueryDefinition query) {
        return computeHitCount(getQueryBuilder().getHostHitCount(), COL_EVENT_HOSTNAME, query.getFrom(), query.getTo());
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getUserHitCount(EventQueryDefinition query) {
        return computeHitCount(getQueryBuilder().getUserHitCount(), COL_EVENT_USER, query.getFrom(), query.getTo());
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getSourceHitCount(EventQueryDefinition query) {
        return computeHitCount(getQueryBuilder().getSourceHitCount(), COL_EVENT_SOURCE, query.getFrom(), query.getTo());
    }

    /** {@inheritDoc} */
    @Override
    public TimeSeriesChart getFeatureUsageHistory(EventQueryDefinition query, TimeUnit units) {
        // Create the interval depending on units
        TimeSeriesChart tsc = new TimeSeriesChart(query.getFrom(), query.getTo(), units);
        // Search All events
        Iterator<Event> iterEvent = searchFeatureUsageEvents(query).iterator();
        // Dispatch events into time slots
        while (iterEvent.hasNext()) {
            tsc.addEvent(iterEvent.next());
        }
        return tsc;
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
