package org.ff4j.jdbc.repository;

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

import static org.ff4j.jdbc.JdbcUtils.closeConnection;
import static org.ff4j.jdbc.JdbcUtils.closeResultSet;
import static org.ff4j.jdbc.JdbcUtils.closeStatement;
import static org.ff4j.jdbc.JdbcUtils.executeUpdate;
import static org.ff4j.jdbc.JdbcUtils.isTableExist;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

import javax.sql.DataSource;

import org.ff4j.event.Event;
import org.ff4j.feature.exception.FeatureAccessException;
import org.ff4j.jdbc.JdbcConstants.AuditTrailColumns;
import org.ff4j.monitoring.AuditTrail;
import org.ff4j.monitoring.AuditTrailQuery;
import org.ff4j.monitoring.HitCount;
import org.ff4j.monitoring.RepositoryEventFeatureUsage;
import org.ff4j.jdbc.JdbcQueryBuilder;

/**
 * Implementation of in memory {@link RepositoryEventFeatureUsage} with limited events.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class AuditTrailJdbc implements AuditTrail {
    
    /** Error message 1. */
    public static final String CANNOT_READ_AUDITTABLE =  "Cannot read audit table from DB";

	/** error message. */
    public static final String CANNOT_BUILD_PIE_CHART_FROM_REPOSITORY = "Cannot build PieChart from repository, ";
    
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
    public AuditTrailJdbc(DataSource jdbcDS) {
        this.dataSource = jdbcDS;
    }
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        DataSource       ds = getDataSource();
        JdbcQueryBuilder qb = getQueryBuilder();
        if (!isTableExist(ds, qb.getTableNameAuditTrail())) {
            executeUpdate(ds, qb.sqlCreateTable(AuditTrailColumns.values()));
        }
    }
    
    @Override
    public void log(Event evt) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public Stream<Event> search(AuditTrailQuery query) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void purge(AuditTrailQuery query) {
        // TODO Auto-generated method stub
        
    }

    
    
    /** {@inheritDoc} *
    
    
    /** {@inheritDoc} *
    @Override
    public void create(Event evt) {
        Util.validateEvent(evt);
        try (Connection sqlConn = dataSource.getConnection()) {
            JdbcEventAuditTrailMapper auditMapper = new JdbcEventAuditTrailMapper(sqlConn, getQueryBuilder());
            try(PreparedStatement stmt = auditMapper.toStore(evt)) {
                stmt.executeUpdate();
            }
        } catch(Exception exc) {
            throw new AuditAccessException("Cannot insert event into DB (" + exc.getClass() + ") "+ exc.getCause(), exc);
        }
    }
    
    /** {@inheritDoc} *
    @Override
    public Optional < Event > findById(String uuid, Long timestamp) {
        Util.requireHasLength(uuid);
        try (Connection sqlConn = dataSource.getConnection()) {
            try(PreparedStatement ps = sqlConn.prepareStatement(getQueryBuilder().sqlSelectAuditById())) {
                ps.setString(1, uuid);
                JdbcEventAuditTrailMapper auditMapper = new JdbcEventAuditTrailMapper(sqlConn, getQueryBuilder());
                try(ResultSet rs = ps.executeQuery()) {
                    return rs.next() ? Optional.of(auditMapper.fromStore(rs)) : Optional.empty();
                }
            }
        } catch (SQLException sqlEX) {
            throw new IllegalStateException("CANNOT_READ_AUDITTABLE", sqlEX);
        }
    }

    /** {@inheritDoc} *
    @Override
    public void delete(String entityId) {
        assertItemExist(entityId);
        try (Connection sqlConn = getDataSource().getConnection()) {
            try(PreparedStatement ps = sqlConn.prepareStatement(getQueryBuilder().sqlDeleteAuditEvent())) {
                ps.setString(1, entityId);
                ps.executeUpdate();
            }
        } catch (SQLException sqlEX) {
            throw new AuditAccessException("CANNOT DELETE EVENT", sqlEX);
        }
    }
    
    /** {@inheritDoc} *
    @Override
    public boolean exists(String uid) {
        requireHasLength(uid);
        Connection          sqlConn = null;
        PreparedStatement   ps = null;
        ResultSet           rs = null;
        try {
            sqlConn = getDataSource().getConnection();
            ps = JdbcUtils.buildStatement(sqlConn, getQueryBuilder().sqlExistEvent(), uid);
            rs = ps.executeQuery();
            rs.next();
            return 1 == rs.getInt(1);
        } catch (SQLException sqlEX) {
            throw new AuditAccessException("Cannot READ EVENT", sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
            closeConnection(sqlConn);
        }
    }

    /** {@inheritDoc} *
    @Override
    public Stream<Event> findAll() {
        Connection          sqlConn = null;
        PreparedStatement   ps = null;
        ResultSet           rs = null;
        try {
            sqlConn = getDataSource().getConnection();
            ps = sqlConn.prepareStatement(getQueryBuilder().sqlFindAllEvents());
            rs = ps.executeQuery();
            List<Event> events = new ArrayList<>();
            while (rs.next()) {
                events.add(new JdbcEventAuditTrailMapper(sqlConn, getQueryBuilder()).fromStore(rs));
            }
            return events.stream();
        } catch (SQLException sqlEX) {
            throw new IllegalStateException("CANNOT_READ_AUDITTABLE", sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
            closeConnection(sqlConn);
        }
    }
    
    /** {@inheritDoc} *
    @Override
    public void purgeAuditTrail(EventQueryDefinition qDef) {
        Util.requireNotNull(qDef);
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
    
    /** {@inheritDoc} *
    @Override
    public void purgeFeatureUsage(EventQueryDefinition qDef) {
        Util.requireNotNull(qDef);
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
    
    /** {@inheritDoc} *
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
                es.add(new JdbcEventAuditTrailMapper(sqlConn, getQueryBuilder()).fromStore(rs));
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
    public  Map<String, HitCount> computeHitCount(String sqlQuery, String columnName, long from, long to) {
        Connection          sqlConn = null;
        PreparedStatement   ps = null;
        ResultSet           rs = null;
        Map<String, HitCount>  hitCount = new HashMap<String, HitCount>();
        try {
            // Returns features
            sqlConn = dataSource.getConnection();
            ps = sqlConn.prepareStatement(sqlQuery);
            ps.setTimestamp(1, new Timestamp(from));
            ps.setTimestamp(2, new Timestamp(to));
            rs = ps.executeQuery();
            while (rs.next()) {
                hitCount.put(rs.getString(columnName), new HitCount(rs.getInt("NB")));
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
    
    /** {@inheritDoc} *
    @Override
    public EventSeries getAuditTrail(EventQueryDefinition qDef) {
        return searchEvents(getQueryBuilder().getSelectAuditTrailQuery(qDef), qDef.getFrom(), qDef.getTo());
    }

    /** {@inheritDoc} *
    @Override
    public EventSeries searchFeatureUsageEvents(EventQueryDefinition qDef) {
        return searchEvents(getQueryBuilder().getSelectFeatureUsageQuery(qDef), qDef.getFrom(), qDef.getTo());
    }
        
    /** {@inheritDoc} *
    @Override
    public Map<String, MutableHitCount> getFeatureUsageHitCount(EventQueryDefinition query) {
        return computeHitCount(getQueryBuilder().getFeaturesHitCount(), COL_EVENT_NAME, query.getFrom(), query.getTo());
    }
    
    /** {@inheritDoc} *
    @Override
    public Map<String, MutableHitCount> getHostHitCount(EventQueryDefinition query) {
        return computeHitCount(getQueryBuilder().getHostHitCount(), COL_EVENT_HOSTNAME, query.getFrom(), query.getTo());
    }

    /** {@inheritDoc} *
    @Override
    public Map<String, MutableHitCount> getUserHitCount(EventQueryDefinition query) {
        return computeHitCount(getQueryBuilder().getUserHitCount(), COL_OWNER, query.getFrom(), query.getTo());
    }

    /** {@inheritDoc} *
    @Override
    public Map<String, MutableHitCount> getSourceHitCount(EventQueryDefinition query) {
        return computeHitCount(getQueryBuilder().getSourceHitCount(), COL_EVENT_SOURCE, query.getFrom(), query.getTo());
    }

    /** {@inheritDoc} *
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
