package org.ff4j.springjdbc.store;

/*
 * #%L
 * ff4j-store-springjdbc
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

import static org.ff4j.audit.EventConstants.ACTION_CHECK_OK;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_HOSTNAME;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_NAME;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_SOURCE;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_USER;

import java.sql.Timestamp;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import javax.sql.DataSource;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.EventSeries;
import org.ff4j.audit.MutableHitCount;
import org.ff4j.audit.chart.TimeSeriesChart;
import org.ff4j.audit.repository.AbstractEventRepository;
import org.ff4j.audit.repository.EventRepository;
import org.ff4j.springjdbc.store.dto.HitCountDto;
import org.ff4j.springjdbc.store.rowmapper.EventRowMapper;
import org.ff4j.springjdbc.store.rowmapper.HitCountRowMapper;
import org.ff4j.store.JdbcQueryBuilder;
import org.ff4j.utils.JdbcUtils;
import org.ff4j.utils.MappingUtil;
import org.ff4j.utils.Util;
import org.springframework.beans.factory.annotation.Required;
import org.springframework.jdbc.core.JdbcTemplate;

/**
 * Implementation of {@link EventRepository} to leverage on Spring Security.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class EventRepositorySpringJdbc extends AbstractEventRepository {

    /** SQL DataSource. */
    private DataSource dataSource;

    /** Access to storage. */
    private JdbcTemplate jdbcTemplate;
    
    /** Query builder. */
    private JdbcQueryBuilder queryBuilder;
    
    /** Mapping fro mresult to bean. */
    private EventRowMapper EVENT_ROWMAPPER = new EventRowMapper();
            
    /**
     * Default constructor.
     */
    public EventRepositorySpringJdbc() {
    }
    
    /**
     * Default constructor.
     */
    public EventRepositorySpringJdbc(DataSource ds) {
        this.dataSource = ds;
    }
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        JdbcQueryBuilder qb = getQueryBuilder();
        if (!JdbcUtils.isTableExist(dataSource, qb.getTableNameAudit())) {
            getJdbcTemplate().update(qb.sqlCreateTableAudit());
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean saveEvent(Event evt) {
        Util.assertEvent(evt);
        return getJdbcTemplate().update(getQueryBuilder().sqlSaveAudit(),
                evt.getUuid(), new java.sql.Timestamp(evt.getTimestamp()), evt.getType(),
                evt.getName(), evt.getAction(),  evt.getHostName(), evt.getSource(),
                evt.getDuration(),  evt.getUser(), evt.getValue(), 
                MappingUtil.fromMap(evt.getCustomKeys())) > 0;
    }

    /** {@inheritDoc} */
    @Override
    public Event getEventByUUID(String uuid, Long timestamp) {
        Util.assertHasLength(uuid);
        List < Event > evts = getJdbcTemplate().query(
                getQueryBuilder().getEventByUuidQuery(), EVENT_ROWMAPPER, uuid);
        return (!evts.isEmpty()) ? evts.get(0) : null;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getFeatureUsageHitCount(EventQueryDefinition query) {
        return computeHitCount(query, COL_EVENT_NAME);
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getHostHitCount(EventQueryDefinition query) {
        return computeHitCount(query, COL_EVENT_HOSTNAME);
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getUserHitCount(EventQueryDefinition query) {
        return computeHitCount(query, COL_EVENT_USER);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getSourceHitCount(EventQueryDefinition query) {
        return computeHitCount(query, COL_EVENT_SOURCE);
    }

    /**
     * Compute over a column.
     *
     * @param query
     *      current query definition
     * @param colName
     *      column name
     * @return
     *      hit count for this value.
     */
    private Map<String, MutableHitCount> computeHitCount(EventQueryDefinition query, String colName) {
        List < HitCountDto> rawResult = getJdbcTemplate().query(
                getQueryBuilder().getHitCount(colName), 
                new HitCountRowMapper(colName), 
                new Timestamp(query.getFrom()), 
                new Timestamp(query.getTo()));
        
        Map<String, MutableHitCount> mapofHitCount = new HashMap<String, MutableHitCount>();
        for (HitCountDto dto : rawResult) {
            mapofHitCount.put(dto.getColumnName(), dto.getHitcount());
        }    
        return mapofHitCount;
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
    
    /** {@inheritDoc} */
    @Override
    public EventSeries searchFeatureUsageEvents(EventQueryDefinition qDef) {
        return searchEvents(
                getQueryBuilder().getSelectFeatureUsageQuery(qDef), 
                qDef.getFrom(), qDef.getTo());
    }

    /** {@inheritDoc} */
    @Override
    public EventSeries getAuditTrail(EventQueryDefinition qDef) {
        return searchEvents(
                getQueryBuilder().getSelectAuditTrailQuery(qDef), 
                qDef.getFrom(), qDef.getTo());
    }
    
    /** {@inheritDoc} */
    private EventSeries searchEvents(String sqlQuery, long from, long to) {
        EventSeries es = new EventSeries();
        es.addAll(getJdbcTemplate().query(sqlQuery, EVENT_ROWMAPPER, 
                new Timestamp(from), new Timestamp(to)));
        return es;
    }

    /** {@inheritDoc} */
    @Override
    public void purgeAuditTrail(EventQueryDefinition query) {
       Util.assertNotNull(query);
       getJdbcTemplate().update(getQueryBuilder().getPurgeAuditTrailQuery(query),
               new java.sql.Timestamp(query.getFrom()),
               new java.sql.Timestamp(query.getTo()));
    }
    
    /** {@inheritDoc} */
    @Override
    public void purgeFeatureUsage(EventQueryDefinition query) {
        Util.assertNotNull(query);
        // Enforce removing events for feature usage 
        query.getActionFilters().add(ACTION_CHECK_OK);
        
        getJdbcTemplate().update(
                getQueryBuilder().getPurgeFeatureUsageQuery(query),
                new java.sql.Timestamp(query.getFrom()),
                new java.sql.Timestamp(query.getTo()));
    }
    
    /**
     * @param dataSource
     *            the dataSource to set
     */
    @Required
    public void setDataSource(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    /**
     * Getter accessor for attribute 'jdbcTemplate'.
     * 
     * @return current value of 'jdbcTemplate'
     */
    public JdbcTemplate getJdbcTemplate() {
        if (jdbcTemplate == null) {
            if (dataSource == null) {
                throw new IllegalStateException("ff4j-jdbc: DatabaseStore has not been properly initialized, datasource is null");
            }
            this.jdbcTemplate = new JdbcTemplate(dataSource);
        }
        return jdbcTemplate;
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
