package org.ff4j.arangodb.store;

/*-
 * #%L
 * ff4j-store-arangodb
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

import com.arangodb.ArangoCollection;
import com.arangodb.ArangoDBException;
import lombok.extern.slf4j.Slf4j;
import org.ff4j.arangodb.StoreMapper;
import org.ff4j.arangodb.document.ArangoDBEvent;
import org.ff4j.audit.Event;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.EventSeries;
import org.ff4j.audit.MutableHitCount;
import org.ff4j.audit.chart.TimeSeriesChart;
import org.ff4j.audit.repository.AbstractEventRepository;
import org.ff4j.utils.Util;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

/**
 * Minimal implementation of event store for ArangoDB.
 *
 * @author nro-dw
 */
@Slf4j
public class EventStoreArangoDB extends AbstractEventRepository {

    private static final String INSERT_EVENT_ERROR = "Error while inserting event";
    private static final String FIND_EVENT_ERROR = "Error while finding event";

    private final GenericArangoDBClient<ArangoDBEvent> eventClient;

    /**
     * @param eventCollection ArangoDB collection for events
     */
    public EventStoreArangoDB(final ArangoCollection eventCollection) {
        this.eventClient = new GenericArangoDBClient<>(eventCollection, ArangoDBEvent.class);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createSchema() {
        eventClient.initSchema();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean saveEvent(final Event e) {
        Util.assertNotNull(e);
        ArangoDBEvent arangoDBEvent = StoreMapper.toEventStore(e);
        return insertEvent(arangoDBEvent);
    }

    private boolean insertEvent(ArangoDBEvent event) {
        try {
            eventClient.insertDocument(event);
            return true;
        } catch (ArangoDBException e) {
            log.error(INSERT_EVENT_ERROR, e);
            return false;
        }
    } 

    /**
     * {@inheritDoc}
     */
    @Override
    public Event getEventByUUID(final String uuid, final Long timestamp) {
        return findEvent(uuid).orElse(null);
    }

    private Optional<Event> findEvent(final String uuid) {
        try {
            Optional<ArangoDBEvent> arangoDBEvent = Optional.ofNullable(eventClient.getDocument(uuid));
            return arangoDBEvent.map(StoreMapper::fromEventStore);
        } catch (ArangoDBException e) {
            log.error(FIND_EVENT_ERROR, e);
            return Optional.empty();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, MutableHitCount> getFeatureUsageHitCount(final EventQueryDefinition query) {
        // Not implemented
        return new HashMap<>();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TimeSeriesChart getFeatureUsageHistory(final EventQueryDefinition query, final TimeUnit tu) {
        // Not implemented
        return new TimeSeriesChart();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public EventSeries searchFeatureUsageEvents(final EventQueryDefinition query) {
        // Not implemented
        return new EventSeries();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void purgeFeatureUsage(final EventQueryDefinition query) {
        // Not implemented
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, MutableHitCount> getHostHitCount(final EventQueryDefinition query) {
        // Not implemented
        return new HashMap<>();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, MutableHitCount> getUserHitCount(final EventQueryDefinition query) {
        // Not implemented
        return new HashMap<>();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, MutableHitCount> getSourceHitCount(final EventQueryDefinition query) {
        // Not implemented
        return new HashMap<>();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public EventSeries getAuditTrail(final EventQueryDefinition query) {
        // Not implemented
        return new EventSeries();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void purgeAuditTrail(final EventQueryDefinition query) {
        // Not implemented
    }
}
