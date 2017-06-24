package org.ff4j.mongo.store;

import java.util.HashMap;
import java.util.HashSet;

/*
 * #%L
 * ff4j-store-mongodb-v3
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

import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.bson.Document;
import org.ff4j.audit.Event;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.EventSeries;
import org.ff4j.audit.MutableHitCount;
import org.ff4j.audit.chart.TimeSeriesChart;
import org.ff4j.audit.repository.AbstractEventRepository;
import org.ff4j.mongo.MongoDbConstants;
import org.ff4j.mongo.mapper.MongoEventMapper;

import com.mongodb.MongoClient;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;

/**
 * Implementation of EventRepository for Mongo.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class EventRepositoryMongo extends AbstractEventRepository {
    
    /** Event Mapping. */
    private static final MongoEventMapper EMAPPER = new MongoEventMapper();
    
    /** MongoDB collection. */
    private MongoCollection<Document> eventsCollection;
    
    /** Feature collection Name. */
    private String collectionName = MongoDbConstants.DEFAULT_EVENT_COLLECTION;
    
    /** Database name. */
    private String dbName = MongoDbConstants.DEFAULT_DBNAME;
    
    /** Current mongo client. */
    private MongoClient mongoClient;
    
    /**
     * Parameterized constructor with collection.
     * 
     * @param collection
     *            the collection to set
     */
    public EventRepositoryMongo(MongoClient client) {
        this(client, MongoDbConstants.DEFAULT_DBNAME);
    }
    
    /**
     * Parameterized constructor with collection.
     * 
     * @param collection
     *            the collection to set
     */
    public EventRepositoryMongo(MongoClient client, String dbName) {
        this.dbName      = dbName;
        this.mongoClient = client;
        this.eventsCollection = getEventCollection();
    }
    
    /**
     * Parameterized constructor with collection.
     * 
     * @param collection
     *            the collection to set
     */
    public EventRepositoryMongo(MongoCollection<Document> events) {
        this.eventsCollection = events;
    }       
    
    /**
     * Parameterized constructor with collection.
     * 
     * @param collection
     *            the collection to set
     */
    public EventRepositoryMongo(MongoDatabase db) {
        this(db, MongoDbConstants.DEFAULT_EVENT_COLLECTION);
    }
    
    /**
     * Parameterized constructor with collection.
     * 
     * @param collection
     *            the collection to set
     */
    public EventRepositoryMongo(MongoDatabase db, String collectionName) {
        this.eventsCollection = db.getCollection(collectionName);
    }
    
    /**
     * Getter accessor for attribute 'featuresCollection'.
     *
     * @return
     *       current value of 'featuresCollection'
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
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        if (!mongoClient.getDatabase(dbName)
                .listCollectionNames()
                .into(new HashSet<String>())
                .contains(collectionName)) {
            mongoClient.getDatabase(dbName).createCollection(collectionName);
        }
        eventsCollection = mongoClient.getDatabase(dbName).getCollection(collectionName);
    }
    
    
    /** {@inheritDoc} */
    @Override
    public boolean saveEvent(Event e) {
        if (e == null) {
            throw new IllegalArgumentException("Event cannot be null nor empty");
        }
        eventsCollection.insertOne(EMAPPER.toStore(e));
        return true;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getFeatureUsageHitCount(EventQueryDefinition query) {
        return new HashMap<String, MutableHitCount>();
    }

    /** {@inheritDoc} */
    @Override
    public TimeSeriesChart getFeatureUsageHistory(EventQueryDefinition query, TimeUnit tu) {
        return new TimeSeriesChart();
    }

    /** {@inheritDoc} */
    @Override
    public EventSeries searchFeatureUsageEvents(EventQueryDefinition query) {        
        return new EventSeries();
    }

    /** {@inheritDoc} */
    @Override
    public void purgeFeatureUsage(EventQueryDefinition query) {
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getHostHitCount(EventQueryDefinition query) {
        return new HashMap<String, MutableHitCount>();
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getUserHitCount(EventQueryDefinition query) {
        return new HashMap<String, MutableHitCount>();
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getSourceHitCount(EventQueryDefinition query) {
        return new HashMap<String, MutableHitCount>();
    }

    /** {@inheritDoc} */
    @Override
    public EventSeries getAuditTrail(EventQueryDefinition query) {
        return new EventSeries();
    }

    /** {@inheritDoc} */
    @Override
    public void purgeAuditTrail(EventQueryDefinition query) {
    }

    /** {@inheritDoc} */
    @Override
    public Event getEventByUUID(String uuid, Long timestamp) {
        return null;
    }
    
}
