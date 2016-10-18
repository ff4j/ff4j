package org.ff4j.audit.repository;

/*
 * #%L
 * ff4j-core
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


import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventConstants;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.EventSeries;
import org.ff4j.audit.MutableHitCount;
import org.ff4j.audit.chart.Serie;
import org.ff4j.audit.chart.TimeSeriesChart;
import org.ff4j.utils.Util;

/**
 * Implementation of in memory {@link EventRepository} with limited events.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class InMemoryEventRepository extends AbstractEventRepository {

    /** default retention. */
    private static final int DEFAULT_QUEUE_CAPACITY = 100000;

    /** current capacity. */
    private int queueCapacity = DEFAULT_QUEUE_CAPACITY;

    /** Event <YYYYMMDD> / <featureUID> -> <Event> list (only action CHECK_ON) */
    private Map<String, Map<String, EventSeries>> featureUsageEvents = new ConcurrentHashMap<String, Map<String, EventSeries>>();

    /** Event <YYYYMMDD> -> <featureUID> -> <Event> list (only action CHECK_OFF) */
    private Map<String, Map<String, EventSeries>> checkOffEvents = new ConcurrentHashMap<String, Map<String, EventSeries>>();

    /** Event <YYYYMMDD> -> Event related to user action in console (not featureUsage, not check OFF). */
    private Map<String, EventSeries> auditTrailEvents = new ConcurrentHashMap<String, EventSeries>();

    /**
     * Default constructor with default capacity to 100.000
     */
    public InMemoryEventRepository() {
        this(DEFAULT_QUEUE_CAPACITY);
    }

    /**
     * Constructor to tune capacity.
     * 
     * @param queueCapacity
     *            default queue capacity
     */
    public InMemoryEventRepository(int queueCapacity) {
        this.queueCapacity = queueCapacity;
    }
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        // There is nothing to create for inMemeory store
        return;
    }

    /** {@inheritDoc} */
    @Override
    public boolean saveEvent(Event e) {
        Util.assertEvent(e);
        if (EventConstants.ACTION_CHECK_OK.equalsIgnoreCase(e.getAction())) {
            return saveEvent(e, featureUsageEvents);
        } else if (EventConstants.ACTION_CHECK_OFF.equalsIgnoreCase(e.getAction())) {
            return saveEvent(e, checkOffEvents);
        }
        String key = getKeyDate(e.getTimestamp());
        if (!auditTrailEvents.containsKey(key)) {
            auditTrailEvents.put(key, new EventSeries(this.queueCapacity));
        }
        return auditTrailEvents.get(key).add(e);
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getFeatureUsageHitCount(EventQueryDefinition query) {
        Map<String, MutableHitCount> hitRatio = new TreeMap<String, MutableHitCount>();
        for (Event event : searchFeatureUsageEvents(query)) {
            if (!hitRatio.containsKey(event.getName())) {
                hitRatio.put(event.getName(), new MutableHitCount());
             }
             hitRatio.get(event.getName()).inc();
        }
        return hitRatio;
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getSourceHitCount(EventQueryDefinition query) {
        Map<String, MutableHitCount> hitRatio = new TreeMap<String, MutableHitCount>();
        for (Event event : searchFeatureUsageEvents(query)) {
            if (!hitRatio.containsKey(event.getSource())) {
                hitRatio.put(event.getSource(), new MutableHitCount());
             }
             hitRatio.get(event.getSource()).inc();
        }
        return hitRatio;
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getHostHitCount(EventQueryDefinition query) {
        Map<String, MutableHitCount> hitRatio = new TreeMap<String, MutableHitCount>();
        for (Event event : searchFeatureUsageEvents(query)) {
            if (!hitRatio.containsKey(event.getHostName())) {
                hitRatio.put(event.getHostName(), new MutableHitCount());
             }
             hitRatio.get(event.getHostName()).inc();
        }
        return hitRatio;
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getUserHitCount(EventQueryDefinition query) {
        Map<String, MutableHitCount> hitRatio = new TreeMap<String, MutableHitCount>();
        for (Event event : searchFeatureUsageEvents(query)) {
            String user = Util.hasLength(event.getUser()) ? event.getUser() : "anonymous";
            if (!hitRatio.containsKey(user)) {
                hitRatio.put(user, new MutableHitCount());
             }
             hitRatio.get(user).inc();
        }
        return hitRatio;
    }
    

    /**
     * Save event to target (based on ACTION).
     *
     * @param e
     *            current event
     * @param target
     *            target list
     * @return if the evetn is stored
     */
    private boolean saveEvent(Event e, Map<String, Map<String, EventSeries>> target) {
        String key = getKeyDate(e.getTimestamp());
        String uid = e.getName();
        if (!target.containsKey(key)) {
            target.put(key, new ConcurrentHashMap<String, EventSeries>());
        }
        if (!target.get(key).containsKey(uid)) {
            target.get(key).put(uid, new EventSeries(this.queueCapacity));
        }
        return target.get(key).get(uid).add(e);
    }
    
   
    
    /** {@inheritDoc} */
    @Override
    public TimeSeriesChart getFeatureUsageHistory(EventQueryDefinition query, TimeUnit units) {
        // Create the interval depending on units
        TimeSeriesChart tsc = new TimeSeriesChart(query.getFrom(), query.getTo(), units);
        
        for (String currentDay : getCandidateDays(query.getFrom(), query.getTo())) {
            // There are some event this day
            if (featureUsageEvents.containsKey(currentDay)) {
                for (Map.Entry<String, EventSeries> entry : featureUsageEvents.get(currentDay).entrySet()) {
                    String currentFeatureName = entry.getKey();
                    // Filter feature names if required
                    Set < String > filteredFeatures = query.getNamesFilter();
                    if (filteredFeatures == null || filteredFeatures.isEmpty() || filteredFeatures.contains(currentFeatureName)) {
                        // Loop over events
                        for (Event evt : entry.getValue()) {
                            // Between bounds (keydate not enough)
                            if (isEventInInterval(evt, query.getFrom(), query.getTo())) {
                                // Create new serie if new feature Name
                                if (!tsc.getSeries().containsKey((currentFeatureName))) {
                                    tsc.createNewSerie(currentFeatureName);
                                }
                                // Match FeatureName
                                Serie < Map<String , MutableHitCount > > serie = tsc.getSeries().get(currentFeatureName);
                                // Match SlotName
                                String slotName = tsc.getSdf().format(new Date(evt.getTimestamp()));
                                // Should be always 'true' as the tsc.getsdf().format() will get a slotName.
                                if (serie.getValue().containsKey(slotName)) {
                                    // Fast Increment
                                    serie.getValue().get(slotName).inc();
                                }
                            }
                        }
                    }
                }
            }
        }
        
        // Recolor series
        List < String > colors = Util.generateHSVGradient("ee1100", "442299", tsc.getSeries().size());
        int idxColor = 0;
        for (Map.Entry<String, Serie<Map<String, MutableHitCount>>> serie : tsc.getSeries().entrySet()) {
            serie.getValue().setColor(colors.get(idxColor));
            idxColor++;
        }
        return tsc;
    }
    
    /** {@inheritDoc} */
    @Override
    public EventSeries getAuditTrail(EventQueryDefinition q) {
        EventSeries resultSeries = new EventSeries(10000);
        for (String currentDay : getCandidateDays(q.getFrom(), q.getTo())) {
            if (auditTrailEvents.containsKey(currentDay)) {
                Iterator<Event> iterEvents = auditTrailEvents.get(currentDay).iterator();
                while (iterEvents.hasNext()) {
                    Event evt = iterEvents.next();
                    if (q.match(evt)) {
                        resultSeries.add(evt);
                    }
                }
            }
        }
        return resultSeries;
    }
    
    /** {@inheritDoc} */
    @Override
    public void purgeAuditTrail(EventQueryDefinition q) {
        for (String currentDay : getCandidateDays(q.getFrom(), q.getTo())) {
            if (auditTrailEvents.containsKey(currentDay)) {
                Iterator<Event> iterEvents = auditTrailEvents.get(currentDay).iterator();
                while (iterEvents.hasNext()) {
                    Event evt = iterEvents.next();
                    if (q.match(evt)) {
                        auditTrailEvents.get(currentDay).remove(evt);
                    }
                }
                if (auditTrailEvents.get(currentDay).isEmpty()) {
                    auditTrailEvents.remove(currentDay);
                }
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void purgeFeatureUsage(EventQueryDefinition q) {
        Set<String> candidateDates = getCandidateDays(q.getFrom(), q.getTo());
        for (String currentDay : candidateDates) {
            if (featureUsageEvents.containsKey(currentDay)) {
                Map<String, EventSeries> currentDayEvents = featureUsageEvents.get(currentDay);
                for (String currentFeature : currentDayEvents.keySet()) {
                    Iterator<Event> iterEvents = currentDayEvents.get(currentFeature).iterator();
                    while (iterEvents.hasNext()) {
                        Event evt = iterEvents.next();
                        if (q.match(evt)) {
                            removeEventIfPresent(currentDayEvents.get(currentFeature), evt);
                        }
                        if (currentDayEvents.get(currentFeature).isEmpty()){
                            currentDayEvents.remove(currentFeature);
                        }
                    }
                }
                // Remove list if empty
                if (currentDayEvents.isEmpty()) {
                    featureUsageEvents.remove(currentDay);
                }
            }
        }
    }
    
    private void removeEventIfPresent(EventSeries es, Event evt) {
        Iterator < Event > iterEvt = es.iterator();
        while(iterEvt.hasNext()) {
            Event currentEvent = iterEvt.next();
            if (currentEvent.getUuid().equalsIgnoreCase(evt.getUuid())) {
                es.remove(currentEvent);
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public EventSeries searchFeatureUsageEvents(EventQueryDefinition query) {
        EventSeries es = new EventSeries(1000000);
        // Dates are the keys of the storage map, compute list of keys and loop over them
        for (String currentDay : getCandidateDays(query.getFrom(), query.getTo())) {
            // There are some events with the current date
            if (featureUsageEvents.containsKey(currentDay)) {
               Map<String, EventSeries> currentDayEvents = featureUsageEvents.get(currentDay);
               for (String currentFeature : currentDayEvents.keySet()) {
                    // query can have filters for names, here we limite the number of map to scan
                    if (query.matchName(currentFeature)) {
                        Iterator<Event> iterEvents = currentDayEvents.get(currentFeature).iterator();
                        while (iterEvents.hasNext()) {
                            Event evt = iterEvents.next();
                            // use other filter (host, action, timestamp....)
                            if (query.match(evt)) {
                                es.add(evt);
                            }
                        }
                    }
                }
            }
        }
        return es;
    }
    
    /** {@inheritDoc} */
    @Override
    public Event getEventByUUID(String uuid, Long timestamp) {
        // Limited Search by key
        if (timestamp != null) {
            String targetDate = KDF.format(new Date(timestamp.longValue()));
            return searchEventById(uuid, targetDate);
            
        } else {
            // Full search
            Set < String > searchedDate = new HashSet<String>();
            for(String currentDate : auditTrailEvents.keySet()) {
                if (!searchedDate.contains(currentDate)) {
                    Event evt = searchEventById(uuid, currentDate);
                    if (evt != null) {
                        return evt;
                    }
                }
                searchedDate.add(currentDate);
            }
            for(String currentDate : featureUsageEvents.keySet()) {
                if (!searchedDate.contains(currentDate)) {
                    Event evt = searchEventById(uuid, currentDate);
                    if (evt != null) {
                        return evt;
                    }
                }
                searchedDate.add(currentDate);
            }
            for(String currentDate : checkOffEvents.keySet()) {
                if (!searchedDate.contains(currentDate)) {
                    Event evt = searchEventById(uuid, currentDate);
                    if (evt != null) {
                        return evt;
                    }
                }
                searchedDate.add(currentDate);
            }
        }
        return null;
    }
    
    /**
     * Given a date fetch in all the list to find the Event.
     *
     * @param uuid
     *      current event unique identifier
     * @param targetDate
     *      target date
     * @return
     *      event if found
     */
    private Event searchEventById(String uuid, String targetDate) {
        Util.assertNotNull(targetDate, uuid);
        // Audit
        Event evt = getFromEventSeries(auditTrailEvents.get(targetDate), uuid);
        if (evt != null) { 
            return evt;
        }
        // FeatureUsage
        Map < String, EventSeries > maOfFeaturesIsages= featureUsageEvents.get(targetDate);
        if (maOfFeaturesIsages != null ) {
            for (EventSeries es : maOfFeaturesIsages.values()) {
                evt = getFromEventSeries(es, uuid);
                if (evt != null) { 
                    return evt;
                }
            }
        }
        // CheckOff
        Map < String, EventSeries > maOfChecKoff = checkOffEvents.get(targetDate);
        if (maOfChecKoff != null) {
            for (EventSeries es : maOfChecKoff.values()) {
                evt = getFromEventSeries(es, uuid);
                if (evt != null) { 
                    return evt;
                }
             }
        }
        return evt;
    }
    
    /**
     * Search event by its id in the eventSeries.
     *
     * @param es
     *      current event series
     * @param uuid
     *      current unique identifier
     * @return
     *      event if found, null if not
     */
    private Event getFromEventSeries(EventSeries es, String uuid) {
        if (es == null) return null;
        Iterator<Event> iterEvents = es.iterator();
        while (iterEvents.hasNext()) {
            Event evt = iterEvents.next();
            if (evt.getUuid().equalsIgnoreCase(uuid)) {
                return evt;
            }
        }
        return null;
    }

    
   
}
