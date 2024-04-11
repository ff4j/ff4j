package org.ff4j.mongo.store;

/*-
 * #%L
 * ff4j-store-mongodb
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

import static org.ff4j.audit.EventConstants.ATTRIBUTE_HOST;
import static org.ff4j.audit.EventConstants.ATTRIBUTE_NAME;
import static org.ff4j.audit.EventConstants.ATTRIBUTE_SOURCE;
import static org.ff4j.audit.EventConstants.ATTRIBUTE_USER;
import static org.ff4j.mongo.MongoDbConstants.EVENT_UUID;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.bson.Document;
import org.bson.conversions.Bson;
import org.ff4j.audit.Event;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.EventSeries;
import org.ff4j.audit.MutableHitCount;
import org.ff4j.audit.chart.TimeSeriesChart;
import org.ff4j.audit.repository.AbstractEventRepository;
import org.ff4j.exception.AuditAccessException;
import org.ff4j.mongo.MongoDbConstants;
import org.ff4j.mongo.mapper.EventDocumentBuilder;
import org.ff4j.mongo.mapper.MongoEventMapper;
import org.ff4j.utils.Util;

import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;
import com.mongodb.client.model.Filters;

/**
 * Implementation of EventRepository for Mongo.
 *
 * @author Cedrick LUNVEN (@clunven)
 * @author Curtis White (@drizztguen77)
 */
public class EventRepositoryMongo extends AbstractEventRepository {

    /**
     * Event Mapping.
     */
    private static final MongoEventMapper eventMapper = new MongoEventMapper();

    /**
     * Build fields.
     */
    private static final EventDocumentBuilder eventDocumentBuilder = new EventDocumentBuilder();

    /**
     * MongoDB collection.
     */
    private MongoCollection<Document> eventsCollection;

    /**
     * Feature collection Name.
     */
    private static final String collectionName = MongoDbConstants.DEFAULT_EVENT_COLLECTION;

    /**
     * error message.
     */
    public static final String EVENT_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY = "Event identifier cannot be null nor empty";

    /**
     * Database name.
     */
    private String dbName = MongoDbConstants.DEFAULT_DBNAME;

    /**
     * Current mongo client.
     */
    private MongoClient mongoClient;

    /**
     * Parameterized constructor with collection.
     *
     * @param client the mongo client
     */
    public EventRepositoryMongo(MongoClient client) {
        this(client, MongoDbConstants.DEFAULT_DBNAME);
    }

    /**
     * Parameterized constructor with collection.
     *
     * @param client the mongo client
     * @param dbName the database name
     */
    public EventRepositoryMongo(MongoClient client, String dbName) {
        this.dbName = dbName;
        this.mongoClient = client;
        this.eventsCollection = getEventCollection();
    }

    /**
     * Parameterized constructor with collection.
     *
     * @param events a list of events
     */
    public EventRepositoryMongo(MongoCollection<Document> events) {
        this.eventsCollection = events;
    }

    /**
     * Parameterized constructor with collection.
     *
     * @param db the mongo database
     */
    public EventRepositoryMongo(MongoDatabase db) {
        this(db, MongoDbConstants.DEFAULT_EVENT_COLLECTION);
    }

    /**
     * Parameterized constructor with collection.
     *
     * @param db             the mongo database
     * @param collectionName collection name
     */
    public EventRepositoryMongo(MongoDatabase db, String collectionName) {
        this.eventsCollection = db.getCollection(collectionName);
    }

    /**
     * Getter accessor for attribute 'featuresCollection'.
     *
     * @return current value of 'featuresCollection'
     */
    public MongoCollection<Document> getEventCollection() {
        if (eventsCollection == null) {
            if (mongoClient != null) {
                createSchema();
            } else {
                throw new IllegalStateException("Cannot initialize Features collection : no mongo client defined");
            }
        }
        return eventsCollection;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createSchema() {
        if (!mongoClient.getDatabase(dbName)
                .listCollectionNames()
                .into(new HashSet<>())
                .contains(collectionName)) {
            mongoClient.getDatabase(dbName).createCollection(collectionName);
        }
        eventsCollection = mongoClient.getDatabase(dbName).getCollection(collectionName);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean saveEvent(Event e) {
        if (e == null) {
            throw new IllegalArgumentException("Event cannot be null nor empty");
        }
        eventsCollection.insertOne(eventMapper.toStore(e));
        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Event getEventByUUID(String uuid, Long timestamp) {
        if (uuid == null || uuid.isEmpty()) {
            throw new IllegalArgumentException(EVENT_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        Document object = getEventCollection().find(eventDocumentBuilder.getEventUuid(uuid)).first();
        if (object == null) {
            throw new AuditAccessException(uuid);
        }
        return eventMapper.fromStore(object);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, MutableHitCount> getFeatureUsageHitCount(EventQueryDefinition query) {
        return computeHitCount(query, ATTRIBUTE_NAME);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, MutableHitCount> getHostHitCount(EventQueryDefinition query) {
        return computeHitCount(query, ATTRIBUTE_HOST);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, MutableHitCount> getUserHitCount(EventQueryDefinition query) {
        return computeHitCount(query, ATTRIBUTE_USER);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, MutableHitCount> getSourceHitCount(EventQueryDefinition query) {
        return computeHitCount(query, ATTRIBUTE_SOURCE);
    }

    /**
     * {@inheritDoc}
     */
    private Map<String, MutableHitCount> computeHitCount(EventQueryDefinition query, String attr) {

        Map<String, MutableHitCount> mapofHitCount = new HashMap<>();
        getEventCollection().aggregate(eventDocumentBuilder.buildHitCountFilters(query, attr)
        ).forEach(document -> {
            if (null != document.get(EVENT_UUID)) {
                mapofHitCount.put(document.get(EVENT_UUID).toString(), new MutableHitCount((Integer) document.get("NB")));
            } else {
                mapofHitCount.put(attr, new MutableHitCount(0));
            }
        });

        return mapofHitCount;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TimeSeriesChart getFeatureUsageHistory(EventQueryDefinition query, TimeUnit tu) {
        // Create the interval depending on units
        TimeSeriesChart tsc = new TimeSeriesChart(query.getFrom(), query.getTo(), tu);
        // Search All events
        // Dispatch events into time slots
        for (Event event : searchFeatureUsageEvents(query)) {
            tsc.addEvent(event);
        }
        return tsc;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public EventSeries searchFeatureUsageEvents(EventQueryDefinition qDef) {
        return searchEvents(eventDocumentBuilder.getSelectFeatureUsageFilters(qDef));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public EventSeries getAuditTrail(EventQueryDefinition qDef) {
        return searchEvents(eventDocumentBuilder.getSelectAuditTrailFilters(qDef));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void purgeAuditTrail(EventQueryDefinition qDef) {
        Util.assertNotNull(qDef);
        getEventCollection().deleteMany(Filters.and(eventDocumentBuilder.getPurgeAuditTrailFilters(qDef)));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void purgeFeatureUsage(EventQueryDefinition qDef) {
        Util.assertNotNull(qDef);
        // Enforce removing events for feature usage
        getEventCollection().deleteMany(Filters.and(eventDocumentBuilder.getPurgeFeatureUsageFilters(qDef)));
    }

    /**
     * Search for events based on filters
     *
     * @param filters filters
     * @return event series
     */
    private EventSeries searchEvents(List<Bson> filters) {
        EventSeries es = new EventSeries();

        getEventCollection().find(Filters.and(filters))
                .forEach(document -> es.add(eventMapper.fromStore(document)));

        return es;
    }
}
