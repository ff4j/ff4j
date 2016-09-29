package org.ff4j.mongo.store;

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
    
    
    /** {@inheritDoc} */
    @Override
    public boolean saveEvent(Event e) {
        if (e == null) {
            throw new IllegalArgumentException("Event cannot be null nor empty");
        }
        eventsCollection.insertOne(EMAPPER.toStore(e));
        return true;
    }

    @Override
    public Map<String, MutableHitCount> getFeatureUsageHitCount(EventQueryDefinition query) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public TimeSeriesChart getFeatureUsageHistory(EventQueryDefinition query, TimeUnit tu) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public EventSeries searchFeatureUsageEvents(EventQueryDefinition query) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void purgeFeatureUsage(EventQueryDefinition query) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public Map<String, MutableHitCount> getHostHitCount(EventQueryDefinition query) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Map<String, MutableHitCount> getUserHitCount(EventQueryDefinition query) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Map<String, MutableHitCount> getSourceHitCount(EventQueryDefinition query) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public EventSeries getAuditTrail(EventQueryDefinition query) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void purgeAuditTrail(EventQueryDefinition query) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public Event getEventByUUID(String uuid, Long timestamp) {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        // TODO Auto-generated method stub
        
    }
}
