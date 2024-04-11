package org.ff4j.cassandra.store;

/*-
 * #%L
 * ff4j-store-cassandra
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventConstants;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.EventSeries;
import org.ff4j.audit.MutableHitCount;
import org.ff4j.audit.chart.TimeSeriesChart;
import org.ff4j.audit.repository.AbstractEventRepository;
import org.ff4j.cassandra.FF4jCassandraSchema;
import org.ff4j.utils.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.datastax.oss.driver.api.core.ConsistencyLevel;
import com.datastax.oss.driver.api.core.CqlSession;
import com.datastax.oss.driver.api.core.cql.BatchStatementBuilder;
import com.datastax.oss.driver.api.core.cql.BatchType;
import com.datastax.oss.driver.api.core.cql.BoundStatement;
import com.datastax.oss.driver.api.core.cql.PreparedStatement;
import com.datastax.oss.driver.api.core.cql.ResultSet;
import com.datastax.oss.driver.api.core.cql.Row;
import com.datastax.oss.driver.api.core.cql.SimpleStatement;
import com.datastax.oss.driver.api.core.cql.SimpleStatementBuilder;

/**
 * Implementation of audit into Cassandra DB
 *
 * @Abstract as note implemented yet.
 * 
 * @author Cedrick LUNVEN (@clunven)
 */
public class EventRepositoryCassandra extends AbstractEventRepository implements FF4jCassandraSchema {
    
    /** logger for this store. */
    private static Logger LOGGER = LoggerFactory.getLogger(EventRepositoryCassandra.class);
    
    /** Driver Session. */
    private CqlSession cqlSession;
    
    /** Default read timeout. */
    private Duration duration = Duration.ofMinutes(10);
    
    /** Static statements on events. */ 
    private PreparedStatement psInsertEvent;
    private PreparedStatement psInsertEventByType;
    private PreparedStatement psReadEventById;
    
    /**
     * Default constructor.
     */
    public EventRepositoryCassandra() {}
    
    /**
     * Connector with running session
     */
    public EventRepositoryCassandra(CqlSession cqlSession) {
        this.cqlSession = cqlSession;
    }
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        cqlSession.execute(STMT_CREATE_TABLE_AUDIT);
        cqlSession.execute(STMT_CREATE_TABLE_AUDITHITCOUNT);
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean saveEvent(Event e) {
        Util.assertEvent(e);
        CqlSession cqlSession = getCqlSession();
        BatchStatementBuilder batchBuilder = new BatchStatementBuilder(BatchType.LOGGED);
        
        BoundStatement bsInsertEvent = psInsertEvent.bind();
        bsInsertEvent = bsInsertEvent.setUuid(AUDIT_ATT_UID, UUID.fromString(e.getUuid()));
        bsInsertEvent = bsInsertEvent.setString(AUDIT_ATT_TYPE, e.getType());
        bsInsertEvent = bsInsertEvent.setInstant(AUDIT_ATT_TIME, Instant.ofEpochMilli(e.getTimestamp()));
        bsInsertEvent = bsInsertEvent.setString(AUDIT_ATT_NAME, e.getName());
        bsInsertEvent = bsInsertEvent.setString(AUDIT_ATT_ACTION, e.getAction());
        bsInsertEvent = bsInsertEvent.setString(AUDIT_ATT_SOURCE, e.getSource());
        bsInsertEvent = bsInsertEvent.setString(AUDIT_ATT_HOSTNAME, e.getHostName());
        bsInsertEvent = bsInsertEvent.setInt(AUDIT_ATT_DURATION, new Long(e.getDuration()).intValue());
        bsInsertEvent = bsInsertEvent.setString(AUDIT_ATT_USER, e.getUser());
        bsInsertEvent = bsInsertEvent.setString(AUDIT_ATT_VALUE, e.getValue());
        bsInsertEvent = bsInsertEvent.setMap(AUDIT_ATT_CUSTOM, e.getCustomKeys(), String.class, String.class);
        batchBuilder.addStatement(bsInsertEvent);
        
        if (EventConstants.ACTION_CHECK_OK.equalsIgnoreCase(e.getAction())) {
            BoundStatement bsInsertEventByType = psInsertEventByType.bind();
            bsInsertEventByType = bsInsertEventByType.setUuid(AUDIT_ATT_UID, UUID.fromString(e.getUuid()));
            bsInsertEventByType = bsInsertEventByType.setInstant(AUDIT_ATT_TIME, Instant.ofEpochMilli(e.getTimestamp()));
            bsInsertEventByType = bsInsertEventByType.setString(AUDIT_ATT_NAME, e.getName());
            bsInsertEventByType = bsInsertEventByType.setString(AUDIT_ATT_SOURCE, e.getSource());
            bsInsertEventByType = bsInsertEventByType.setString(AUDIT_ATT_HOSTNAME, e.getHostName());
            bsInsertEventByType = bsInsertEventByType.setInt(AUDIT_ATT_DURATION, new Long(e.getDuration()).intValue());
            bsInsertEventByType = bsInsertEventByType.setString(AUDIT_ATT_USER, e.getUser());
            bsInsertEventByType = bsInsertEventByType.setString(AUDIT_ATT_VALUE, e.getValue());
            bsInsertEventByType = bsInsertEventByType.setMap(AUDIT_ATT_CUSTOM, e.getCustomKeys(), String.class, String.class);
            batchBuilder.addStatement(bsInsertEventByType);
        }
        
        cqlSession.execute(batchBuilder.build());
        return true;
    }   

    /** {@inheritDoc} */
    @Override
    public Event getEventByUUID(String uuid, Long timestamp) {
        Util.assertHasLength(uuid);
        BoundStatement stmtFindEventById = psReadEventById.bind(UUID.fromString(uuid));
        ResultSet rs = getCqlSession().execute(stmtFindEventById);
        Row row = rs.one();
        if (null == row) {
            /* 
             * Was not in table audit, so maybe hitcount but need allow filtering
             * It will be slow no need to prepare :p (full scan)
             */
            SimpleStatement ss = new SimpleStatementBuilder(
                    "SELECT * FROM " + AUDIT_HITCOUNT_TABLE 
                 + " WHERE " + AUDIT_ATT_UID + "= ?"
                 + " ALLOW FILTERING")
                    .addPositionalValue(UUID.fromString(uuid))
                    .build();
            ResultSet rs2 = getCqlSession().execute(ss);
            Row row2 = rs2.one();
            if (null == row2) {
                return null;
            }
            return mapEventHit(row2);
        }
        return mapEventRow(row);
    }
    
    private SimpleStatement buildDynamicStatementHitCount(EventQueryDefinition query) {
        // Default settings
        StringBuilder sb = new StringBuilder();
        sb.append("SELECT * FROM " + AUDIT_HITCOUNT_TABLE);
        sb.append(" WHERE (" + AUDIT_ATT_TIME + "> ?) ");
        sb.append(" AND   (" + AUDIT_ATT_TIME + "< ?) ");
        
        List<Object> parameters = new ArrayList<>();
        parameters.add(Instant.ofEpochMilli(query.getFrom()));
        parameters.add(Instant.ofEpochMilli(query.getTo()));
        
        // Name is the PARTITION KEY
        if (!query.getNamesFilter().isEmpty()) {
            sb.append(" AND (" + AUDIT_ATT_NAME + " IN ?)");
            parameters.add(query.getNamesFilter());
        }
        if (!query.getActionFilters().isEmpty()) {
            sb.append(" AND (" + AUDIT_ATT_ACTION + " IN ?)");
            parameters.add(query.getActionFilters());
        }
        if (!query.getHostFilters().isEmpty()) {
            sb.append(" AND (" + AUDIT_ATT_HOSTNAME + " IN ?)");
            parameters.add(query.getHostFilters());
        }
        if (!query.getSourceFilters().isEmpty()) {
            sb.append(" AND (" + AUDIT_ATT_SOURCE + " IN ? )");
            parameters.add(query.getSourceFilters());
        }
        sb.append(" ALLOW FILTERING");
        
        SimpleStatementBuilder builder = SimpleStatement.builder(sb.toString());
        for(Object o : parameters) {
            builder = builder.addPositionalValue(o);
        }
        // Accelerate the query
        SimpleStatement ss = builder.build();
        ss.setConsistencyLevel(ConsistencyLevel.ONE);
        ss.setTimeout(Duration.ofMinutes(10));
        ss.setTracing(false);
        return ss;
    }
    
    private SimpleStatement buildDynamicStatementAudit(EventQueryDefinition query) {
        StringBuilder sb = new StringBuilder();
        sb.append("SELECT * FROM " + AUDIT_TABLE);
        sb.append(" WHERE (" + AUDIT_ATT_TIME + "> ?) ");
        sb.append(" AND   (" + AUDIT_ATT_TIME + "< ?) ");
        
        List<Object> parameters = new ArrayList<>();
        parameters.add(Instant.ofEpochMilli(query.getFrom()));
        parameters.add(Instant.ofEpochMilli(query.getTo()));
        
        // Name is the PARTITION KEY
        if (!query.getNamesFilter().isEmpty()) {
            sb.append(" AND (" + AUDIT_ATT_NAME + " IN ?)");
            parameters.add(query.getNamesFilter());
        }
        if (!query.getActionFilters().isEmpty()) {
            sb.append(" AND (" + AUDIT_ATT_ACTION + " IN ?)");
            parameters.add(query.getActionFilters());
        }
        if (!query.getHostFilters().isEmpty()) {
            sb.append(" AND (" + AUDIT_ATT_HOSTNAME + " IN ?)");
            parameters.add(query.getHostFilters());
        }
        if (!query.getSourceFilters().isEmpty()) {
            sb.append(" AND (" + AUDIT_ATT_SOURCE + " IN ? )");
            parameters.add(query.getSourceFilters());
        }
        sb.append(" ALLOW FILTERING");
        
        SimpleStatementBuilder builder = SimpleStatement.builder(sb.toString());
        for(Object o : parameters) {
            builder = builder.addPositionalValue(o);
        }
        // Accelerate the query
        SimpleStatement ss = builder.build();
        ss.setConsistencyLevel(ConsistencyLevel.ONE);
        ss.setTimeout(Duration.ofMinutes(10));
        ss.setTracing(false);
        return ss;
    }
    
    private Map < String, MutableHitCount > countResultSet(ResultSet rs, String dimension) {
        Map < String, MutableHitCount > hitCount = new HashMap<String, MutableHitCount>();
        // Let driver do paging for us 
        for (Row row : rs) {
            String featureName = row.getString(dimension);
            if (hitCount.containsKey(featureName)) {
                hitCount.get(featureName).inc();
            } else {
                hitCount.put(featureName, new MutableHitCount(1));
            }
        }
        return hitCount;
    }
    
    public Map<String, MutableHitCount> executeHitCount(EventQueryDefinition query, String dimension) {
        LOGGER.warn("You are executing an OLAP query. The generic nature of query definition "
                + "does not allow custom tables. The response time could be long (full scan).");
        SimpleStatement ss = buildDynamicStatementHitCount(query);
        LOGGER.info("Executing OLAP: {}", ss.getQuery());
        return countResultSet(getCqlSession().execute(ss), dimension);
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getFeatureUsageHitCount(EventQueryDefinition query) {
        return executeHitCount(query, AUDIT_ATT_NAME);
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getUserHitCount(EventQueryDefinition query) {
        return executeHitCount(query, AUDIT_ATT_USER);
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getHostHitCount(EventQueryDefinition query) {
        return executeHitCount(query, AUDIT_ATT_HOSTNAME);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getSourceHitCount(EventQueryDefinition query) {
        return executeHitCount(query, AUDIT_ATT_SOURCE);
    }

    /** {@inheritDoc} */
    @Override
    public EventSeries getAuditTrail(EventQueryDefinition query) {
        EventSeries es = new EventSeries();
        for (Row row : getCqlSession().execute(buildDynamicStatementAudit(query))) {
           es.add(mapEventRow(row));
        }
        return es;
    }

    /** {@inheritDoc} */
    @Override
    public void purgeFeatureUsage(EventQueryDefinition query) {
        purgeAuditTrail(query);
    }
    
    /** {@inheritDoc} */
    @Override
    public void purgeAuditTrail(EventQueryDefinition query) {
        truncateTable(getCqlSession(), AUDIT_HITCOUNT_TABLE);
        truncateTable(getCqlSession(), AUDIT_TABLE);
    }
    
    /** {@inheritDoc} */
    @Override
    public EventSeries searchFeatureUsageEvents(EventQueryDefinition query) {
        EventSeries es = new EventSeries();
        for (Row row : getCqlSession().execute(buildDynamicStatementHitCount(query))) {
           es.add(mapEventHit(row));
        }
        return es;
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
    
    protected Event mapEventRow(Row row) {
        Event e = new Event();
        e.setAction(row.getString(AUDIT_ATT_ACTION));
        e.setDuration(row.getInt(AUDIT_ATT_DURATION));
        e.setHostName(row.getString(AUDIT_ATT_HOSTNAME));
        e.setName(row.getString(AUDIT_ATT_NAME));
        e.setSource(row.getString(AUDIT_ATT_SOURCE));
        e.setTimestamp(row.getInstant(AUDIT_ATT_TIME).getEpochSecond());
        e.setType(row.getString(AUDIT_ATT_TYPE));
        e.setUser(row.getString(AUDIT_ATT_USER));
        e.setValue(row.getString(AUDIT_ATT_VALUE));
        e.setUuid(row.getUuid(AUDIT_ATT_UID).toString());
        e.setCustomKeys(row.getMap(AUDIT_ATT_CUSTOM, String.class, String.class));
        return e;
    }
    
    protected Event mapEventHit(Row row) {
        Event e = new Event();
        e.setDuration(row.getInt(AUDIT_ATT_DURATION));
        e.setHostName(row.getString(AUDIT_ATT_HOSTNAME));
        e.setName(row.getString(AUDIT_ATT_NAME));
        e.setSource(row.getString(AUDIT_ATT_SOURCE));
        e.setTimestamp(row.getInstant(AUDIT_ATT_TIME).getEpochSecond());
        e.setUser(row.getString(AUDIT_ATT_USER));
        e.setValue(row.getString(AUDIT_ATT_VALUE));
        e.setUuid(row.getUuid(AUDIT_ATT_UID).toString());
        e.setCustomKeys(row.getMap(AUDIT_ATT_CUSTOM, String.class, String.class));
        return e;
    }

    /**
     * Prepared once, run many.
     */
    protected void prepareStatements() {
        psInsertEvent       = cqlSession.prepare(STMT_AUDIT_INSERT);
        psInsertEventByType = cqlSession.prepare(STMT_AUDIT_INSERT_HITCOUNT);
        psReadEventById     = cqlSession.prepare(STMT_AUDIT_READ_BY_ID);
    }
    
    /**
     * Prepared statements on first call.
     */
    private synchronized CqlSession getCqlSession() {
        if (null == psInsertEvent) {
            prepareStatements();
        }
        return cqlSession;
    }

    /**
     * Getter accessor for attribute 'duration'.
     *
     * @return
     *       current value of 'duration'
     */
    public Duration getDuration() {
        return duration;
    }

    /**
     * Setter accessor for attribute 'duration'.
     * @param duration
     * 		new value for 'duration '
     */
    public void setDuration(Duration duration) {
        this.duration = duration;
    }
   
}
