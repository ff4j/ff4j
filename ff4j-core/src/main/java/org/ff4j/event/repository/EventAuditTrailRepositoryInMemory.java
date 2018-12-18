package org.ff4j.event.repository;

/*-
 * #%L
 * ff4j-core
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

import static org.ff4j.test.AssertUtils.*;
import static org.ff4j.utils.Util.validateEvent;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;

import org.ff4j.event.Event;
import org.ff4j.event.EventSeries;
import org.ff4j.event.monitoring.AuditTrailQuery;
import org.ff4j.utils.Util;

/**
 * Store AuditLog for all informations related to features and properties.
 *
 * @author Cedrick LUNVEN  (@clunven)
 */
public class EventAuditTrailRepositoryInMemory implements EventAuditTrailRepository {

    /** default retention. */
    private final int DEFAULT_QUEUE_CAPACITY = 100000;

    /** current capacity. */
    private int queueCapacity = DEFAULT_QUEUE_CAPACITY;
    
    /** Event <SCOPE> -> <ID> -> List Event related to user action in console (not featureUsage, not check OFF). */
    private static Map< String , Map < String, EventSeries>> auditTrail = new ConcurrentHashMap<>();
    
    /** Default constructor. */
    public EventAuditTrailRepositoryInMemory() {}
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {}
    
    /** {@inheritDoc} */
    @Override
    public void log(Event evt) {
        validateEvent(evt);
        String scope = evt.getScope();
        if (!auditTrail.containsKey(scope)) {
            auditTrail.put(scope, new ConcurrentHashMap<>());
        }
        // Some event do not point a specific feature (featureStore..) so reuse the scope
        String uid = Util.hasLength(evt.getTargetUid()) ? evt.getTargetUid() : scope;
        if (!auditTrail.get(scope).containsKey(uid)) {
            auditTrail.get(scope).put(uid, new EventSeries(queueCapacity));
        }
        auditTrail.get(scope).get(uid).add(evt);
    }

    /** {@inheritDoc} */
    @Override
    public Stream<Event> search(AuditTrailQuery query) {
        assertNotNull(query);
        if (query.getScope().isPresent()) {
            // Filter event to get only
            Event.Scope queryScope = query.getScope().get();
            return searchInMapOfEventSeries(query, auditTrail.get(queryScope.toString())).stream();
        }
        Collection < Event > results = new ArrayList<>();
        auditTrail.values().stream().forEach(map -> results.addAll(searchInMapOfEventSeries(query, map)));
        return results.stream();
    }
    
    /**
     * Utility to fetch a map of eventSeries.
     *
     * @param query
     *      current audit query (date, scope)
     * @param mapOfEventSeries
     *      map of events to perform filter
     * @return
     *      the events matching query in the target event series
     */
    private Collection < Event > searchInMapOfEventSeries(AuditTrailQuery query, Map < String, EventSeries > mapOfEventSeries) {
        assertNotNull(query);
        // Map of EventSeries
        if (query.getUid().isPresent()) {
            // Single EventSerie to search
            EventSeries targetSerie = mapOfEventSeries.get(query.getUid().get());
            return query.filter(targetSerie);
        }
        // No single EventSerie so will have to loop on each key
        Collection < Event > results = new ArrayList<>();
        mapOfEventSeries.values().forEach(es -> results.addAll(query.filter(es)));
        return results;
    }

    /** {@inheritDoc} */
    @Override
    public void purge(AuditTrailQuery query) {
        // Will get a stream of event to remove
        search(query).forEach(evt -> {
            // Get correct scope (Feature, Properties, ...)
            auditTrail.get(evt.getScope())
                      // Get correct event series
                      .get(evt.getTargetUid())
                      // Remove from the Event Series
                      .remove(evt);
        });
    }    
    
}
