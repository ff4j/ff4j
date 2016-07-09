package org.ff4j.mongo.store;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
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

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getFeatureUsageHitCount(long startTime, long endTime) {
        return new HashMap<String, MutableHitCount>();
    }

    /** {@inheritDoc} */
    @Override
    public TimeSeriesChart getFeatureUsageHistory(long startTime, long endTime, TimeUnit tu) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public TimeSeriesChart getFeatureUsageHistory(long startTime, long endTime, TimeUnit tu, Set<String> filteredFeatures) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public EventSeries searchFeatureUsageEvents(EventQueryDefinition query) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void purgeFeatureUsage(long starTime, long endTime) {
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getHostHitCount(long startTime, long endTime) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getUserHitCount(long startTime, long endTime) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getSourceHitCount(long startTime, long endTime) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public TimeSeriesChart getAverageResponseTime(long startTime, long endTime, TimeUnit tu) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public TimeSeriesChart getAverageResponseTime(long startTime, long endTime, TimeUnit tu, Set<String> filteredFeatures) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public EventSeries getAuditTrail(long startTime, long endTime) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void purgeAuditTrail(long starTime, long endTime) {
    }

}
