package org.ff4j.cassandra.store;

import static org.ff4j.audit.EventConstants.ACTION_CLEAR;
import static org.ff4j.audit.EventConstants.ACTION_CREATE;
import static org.ff4j.audit.EventConstants.ACTION_DELETE;
import static org.ff4j.audit.EventConstants.ACTION_DISCONNECT;
import static org.ff4j.audit.EventConstants.ACTION_TOGGLE_OFF;
import static org.ff4j.audit.EventConstants.ACTION_TOGGLE_ON;
import static org.ff4j.audit.EventConstants.ACTION_UPDATE;
import static org.ff4j.cassandra.CassandraConstants.COLUMN_FAMILY_AUDIT;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_HOSTNAME;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_NAME;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_SOURCE;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_USER;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_ACTION;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

/*
 * #%L
 * ff4j-store-cassandra
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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

import org.ff4j.audit.Event;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.EventSeries;
import org.ff4j.audit.MutableHitCount;
import org.ff4j.audit.chart.TimeSeriesChart;
import org.ff4j.audit.repository.AbstractEventRepository;
import org.ff4j.cassandra.CassandraConnection;
import org.ff4j.cassandra.CassandraMapper;
import org.ff4j.cassandra.CassandraQueryBuilder;
import org.ff4j.utils.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.datastax.driver.core.ResultSet;
import com.datastax.driver.core.Row;

/**
 * Implementation of audit into Cassandra DB
 *
 * @Abstract as note implemented yet.
 * 
 * @author Cedrick LUNVEN (@clunven)
 */
public class EventRepositoryCassandra extends AbstractEventRepository {
    
    /** logger for this store. */
    private static Logger LOGGER = LoggerFactory.getLogger(EventRepositoryCassandra.class);
    
    /** TTL to working with ' expiring columns' if positive number in SECONDS. */
    private int ttl = -1;
    
    /** Connection to store Cassandra. */
    private CassandraQueryBuilder builder;
            
    /** Connection to store Cassandra. */
    private CassandraConnection conn;
    
    /**
     * Default constructor.
     */
    public EventRepositoryCassandra() {
    }
    
    /**
     * Initialization through {@link CassandraConnection}.
     *
     * @param conn
     *      current client to cassandra db
     */
    public EventRepositoryCassandra(CassandraConnection conn) {
        this.conn = conn;
    }

    /** {@inheritDoc} */
    @Override
    public void createSchema() {
       if (!conn.isColumnFamilyExist(COLUMN_FAMILY_AUDIT)) {
           conn.getSession().execute(getBuilder().cqlCreateColumnFamilyAudit());
           LOGGER.debug("Column Family '{}' created", COLUMN_FAMILY_AUDIT);
       }
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean saveEvent(Event e) {
        Util.assertEvent(e);
        LOGGER.debug("Event Logged {}", e.toJson());
        conn.getSession().execute(getBuilder().cqlCreateEvent(ttl),
                e.getUuid(), KDF.format(e.getDate()), e.getTimestamp(),
                e.getType(), e.getName(), e.getAction(),
                e.getHostName(), e.getSource(), e.getDuration(),
                e.getUser(), e.getValue(), e.getCustomKeys());
        return true;
    }   

    /** {@inheritDoc} */
    @Override
    public Event getEventByUUID(String uuid, Long timestamp) {
        ResultSet rs = conn.getSession().execute(getBuilder().cqlGetEventById(), uuid);
        return CassandraMapper.mapEvent(rs.one());
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getFeatureUsageHitCount(EventQueryDefinition query) {
        String cqlQuery = getBuilder().cqlFeatureUsageHitCount(query);
        LOGGER.debug("Query " + cqlQuery);
        ResultSet rs = conn.getSession().execute(cqlQuery);
        Map < String, MutableHitCount > hitCount = new HashMap<String, MutableHitCount>();
        for (Row row : rs.all()) {
            String featureName = row.getString(COL_EVENT_NAME);
            if (hitCount.containsKey(featureName)) {
                hitCount.get(featureName).inc();
            } else {
                hitCount.put(featureName, new MutableHitCount(1));
            }
        }
        return hitCount;
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getUserHitCount(EventQueryDefinition query) {
        String cqlQuery = getBuilder().cqlUserHitCount(query);
        LOGGER.debug("Query " + cqlQuery);
        ResultSet rs = conn.getSession().execute(cqlQuery);
        Map < String, MutableHitCount > hitCount = new HashMap<String, MutableHitCount>();
        for (Row row : rs.all()) {
            String user = row.getString(COL_EVENT_USER);
            if (hitCount.containsKey(user)) {
                hitCount.get(user).inc();
            } else {
                hitCount.put(user, new MutableHitCount(1));
            }
        }
        return hitCount;
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getHostHitCount(EventQueryDefinition query) {
        String cqlQuery = getBuilder().cqlHostHitCount(query);
        LOGGER.debug("Query " + cqlQuery);
        ResultSet rs = conn.getSession().execute(cqlQuery);
        Map < String, MutableHitCount > hitCount = new HashMap<String, MutableHitCount>();
        for (Row row : rs.all()) {
            String hostName = row.getString(COL_EVENT_HOSTNAME);
            if (hitCount.containsKey(hostName)) {
                hitCount.get(hostName).inc();
            } else {
                hitCount.put(hostName, new MutableHitCount(1));
            }
        }
        return hitCount;
    }    

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getSourceHitCount(EventQueryDefinition query) {
        String cqlQuery = getBuilder().cqlSourceHitCount(query);
        LOGGER.debug("Query " + cqlQuery);
        ResultSet rs = conn.getSession().execute(cqlQuery);
        Map < String, MutableHitCount > hitCount = new HashMap<String, MutableHitCount>();
        for (Row row : rs.all()) {
            String source = row.getString(COL_EVENT_SOURCE);
            if (hitCount.containsKey(source)) {
                hitCount.get(source).inc();
            } else {
                hitCount.put(source, new MutableHitCount(1));
            }
        }
        return hitCount;
    }

    /** {@inheritDoc} */
    @Override
    public EventSeries getAuditTrail(EventQueryDefinition query) {
        String cqlQuery = getBuilder().cqlAuditTrail(query);
        LOGGER.debug("Query " + cqlQuery);
        ResultSet rs = conn.getSession().execute(cqlQuery);
        EventSeries es = new EventSeries();
        Set < String > candidates = Util.set(ACTION_DISCONNECT, 
                ACTION_TOGGLE_ON, ACTION_TOGGLE_OFF,
                ACTION_CREATE, ACTION_DELETE,
                ACTION_UPDATE, ACTION_CLEAR);
        for (Row row : rs.all()) {
            if (candidates.contains(row.getString(COL_EVENT_ACTION)))
            es.add(CassandraMapper.mapEvent(row));
        }
        return es;
    }

    /** {@inheritDoc} */
    @Override
    public void purgeFeatureUsage(EventQueryDefinition query) {
        this.purgeAuditTrail(query);
    }
    
    /** {@inheritDoc} */
    @Override
    public void purgeAuditTrail(EventQueryDefinition query) {
        LOGGER.warn("All audit will be purged, cannot filter");
        conn.getSession().execute(getBuilder().cqlTruncateAudit());
    }
    
    /** {@inheritDoc} */
    @Override
    public EventSeries searchFeatureUsageEvents(EventQueryDefinition query) {
        String cqlQuery = getBuilder().cqlAuditFeatureUsage(query);
        LOGGER.debug("Query " + cqlQuery);
        ResultSet rs = conn.getSession().execute(cqlQuery);
        EventSeries es = new EventSeries();
        for (Row row : rs.all()) {
            es.add(CassandraMapper.mapEvent(row));
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

    /**
     * Getter accessor for attribute 'builder'.
     *
     * @return
     *       current value of 'builder'
     */
    public CassandraQueryBuilder getBuilder() {
        if (builder == null) {
            builder = new CassandraQueryBuilder(conn);
        }
        return builder;
    }

    /**
     * Setter accessor for attribute 'builder'.
     * @param builder
     *      new value for 'builder '
     */
    public void setBuilder(CassandraQueryBuilder builder) {
        this.builder = builder;
    }

    /**
     * Getter accessor for attribute 'ttl'.
     *
     * @return
     *       current value of 'ttl'
     */
    public int getTtl() {
        return ttl;
    }

    /**
     * Setter accessor for attribute 'ttl'.
     * @param ttl
     *      new value for 'ttl '
     */
    public void setTtl(int ttl) {
        this.ttl = ttl;
    }
    
    /**
     * Getter accessor for attribute 'conn'.
     *
     * @return
     *       current value of 'conn'
     */
    public CassandraConnection getConn() {
        return conn;
    }

    /**
     * Setter accessor for attribute 'conn'.
     * @param conn
     *      new value for 'conn '
     */
    public void setConn(CassandraConnection conn) {
        this.conn = conn;
    }
   
}
