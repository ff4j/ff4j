package org.ff4j.gcpdatastore.store.event;

/*-
 * #%L
 * ff4j-store-gcp-datastore
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


import com.google.cloud.datastore.Datastore;
import com.google.cloud.datastore.DatastoreException;
import com.google.cloud.datastore.Entity;
import org.ff4j.audit.Event;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.EventSeries;
import org.ff4j.audit.MutableHitCount;
import org.ff4j.audit.chart.TimeSeriesChart;
import org.ff4j.audit.repository.AbstractEventRepository;
import org.ff4j.gcpdatastore.store.DatastoreClient;
import org.ff4j.gcpdatastore.store.EntityMapper;
import org.ff4j.gcpdatastore.store.StoreMapper;
import org.ff4j.utils.Util;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.TimeUnit;


/**
 * Minimal implementation of event store
 */
public class DatastoreEventRepository extends AbstractEventRepository {
    private static final String DEFAULT_EVENT_STORE_KIND = "Ff4jEvent";

    private final DatastoreClient storeClient;

    /**
     * Constructor with datastore connection
     *
     * @param datastore the database connection
     */
    public DatastoreEventRepository(Datastore datastore) {
        storeClient = new DatastoreClient(datastore, DEFAULT_EVENT_STORE_KIND);
    }

    /**
     * Constructor with datastore connection and Namespace
     *
     * @param datastore the database connection
     * @param namespace the DB namespace in which the default Kind has to be created
     */
    public DatastoreEventRepository(Datastore datastore, String namespace) {
        storeClient = new DatastoreClient(datastore, namespace, DEFAULT_EVENT_STORE_KIND);
    }

    /**
     * Constructor with datastore connection, Namespace and Kind
     *
     * @param datastore the database connection
     * @param namespace the DB namespace in which the Kind has to be created
     * @param kind      the Kind to be created
     */
    public DatastoreEventRepository(Datastore datastore, String namespace, String kind) {
        storeClient = new DatastoreClient(datastore, namespace, kind);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean saveEvent(Event e) {
        Util.assertNotNull(e);

        DatastoreEvent datastoreEvent = StoreMapper.toEventStore(e);
        Entity entity = EntityMapper.toEntity(datastoreEvent, storeClient.getKeyFactory());
        try {
            storeClient.insert(entity);
            return true;
        } catch (DatastoreException ex) {
            return false;
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Event getEventByUUID(String uuid, Long timestamp) {
        Optional<Entity> entity = storeClient.get(uuid);

        return entity
                .map(EntityMapper::fromEventEntity)
                .map(StoreMapper::fromEventStore)
                .orElse(null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, MutableHitCount> getFeatureUsageHitCount(EventQueryDefinition query) {
        // Not implemented
        return new HashMap<>();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TimeSeriesChart getFeatureUsageHistory(EventQueryDefinition query, TimeUnit tu) {
        // Not implemented
        return new TimeSeriesChart();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public EventSeries searchFeatureUsageEvents(EventQueryDefinition query) {
        // Not implemented
        return new EventSeries();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void purgeFeatureUsage(EventQueryDefinition query) {
        // Not implemented
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, MutableHitCount> getHostHitCount(EventQueryDefinition query) {
        // Not implemented
        return new HashMap<>();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, MutableHitCount> getUserHitCount(EventQueryDefinition query) {
        // Not implemented
        return new HashMap<>();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, MutableHitCount> getSourceHitCount(EventQueryDefinition query) {
        // Not implemented
        return new HashMap<>();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public EventSeries getAuditTrail(EventQueryDefinition query) {
        // Not implemented
        return new EventSeries();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void purgeAuditTrail(EventQueryDefinition query) {
        // Not implemented
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createSchema() {
        // No-op
    }
}
