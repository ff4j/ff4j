package org.ff4j.couchbase.store.store;

/*
 * #%L
 * ff4j-store-springcouchbase
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

import com.couchbase.client.java.Bucket;
import org.ff4j.audit.Event;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.EventSeries;
import org.ff4j.audit.MutableHitCount;
import org.ff4j.audit.chart.TimeSeriesChart;
import org.ff4j.audit.repository.AbstractEventRepository;
import org.ff4j.couchbase.store.document.EventDocument;
import org.ff4j.couchbase.store.mapper.DocumentMapper;
import org.ff4j.couchbase.store.repository.CouchbaseRepository;
import org.ff4j.utils.Util;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.Map;
import java.util.concurrent.TimeUnit;

public class EventStoreCouchbase extends AbstractEventRepository {

    /** TTL to working with ' expiring columns' if positive number in SECONDS. */
    private int ttl = -1;

    /** Couchbase bucket connections **/
    private CouchbaseRepository<EventDocument> eventRepository;

    /**
     * Default constructor.
     */
    public EventStoreCouchbase(Bucket propertyBucket) {
        //ObjectMapper objectMapper = ObjectMapperFactory.createMapper();
        //this.eventRepository = new CouchbaseRepository<>(propertyBucket, EventDocument.class, objectMapper);
        throw new NotImplementedException();
    }

    /** {@inheritDoc} */
    @Override
    public void createSchema() {

    }
    
    /** {@inheritDoc} */
    @Override
    public boolean saveEvent(Event e) {
        Util.assertEvent(e);
        eventRepository.upsert(e.getUuid(), DocumentMapper.eventToEventDocument(e));
        return true;
    }   

    /** {@inheritDoc} */
    @Override
    public Event getEventByUUID(String uuid, Long timestamp) {
        return DocumentMapper.eventDocumentToEvent(eventRepository.get(uuid));
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getFeatureUsageHitCount(EventQueryDefinition query) {
        // TODO
        throw new NotImplementedException();
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getUserHitCount(EventQueryDefinition query) {
        // TODO
        throw new NotImplementedException();
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getHostHitCount(EventQueryDefinition query) {
        // TODO
        throw new NotImplementedException();
    }    

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getSourceHitCount(EventQueryDefinition query) {
        // TODO
        throw new NotImplementedException();
    }

    /** {@inheritDoc} */
    @Override
    public EventSeries getAuditTrail(EventQueryDefinition query) {
        // TODO
        throw new NotImplementedException();
    }

    /** {@inheritDoc} */
    @Override
    public void purgeFeatureUsage(EventQueryDefinition query) {
        this.purgeAuditTrail(query);
    }
    
    /** {@inheritDoc} */
    @Override
    public void purgeAuditTrail(EventQueryDefinition query) {
        // TODO
        throw new NotImplementedException();
    }
    
    /** {@inheritDoc} */
    @Override
    public EventSeries searchFeatureUsageEvents(EventQueryDefinition query) {
        // TODO
        throw new NotImplementedException();
    }  

    /** {@inheritDoc} */
    @Override
    public TimeSeriesChart getFeatureUsageHistory(EventQueryDefinition query, TimeUnit units) {
        // TODO
        throw new NotImplementedException();
    }      
}
