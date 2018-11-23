package org.ff4j.inmemory.repository;

import static org.ff4j.test.AssertUtils.assertHasLength;
import static org.ff4j.test.AssertUtils.assertNotNull;

import java.util.ArrayList;
import java.util.Collection;

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
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.ff4j.chart.Serie;
import org.ff4j.chart.TimeSeriesChart;
import org.ff4j.event.Event;
import org.ff4j.event.EventQueryDefinition;
import org.ff4j.event.EventSeries;
import org.ff4j.event.repo.AbstractRepositoryFeatureUsage;
import org.ff4j.event.repo.RepositoryEventFeatureUsage;
import org.ff4j.monitoring.HitCount;

/**
 * Implementation of in memory {@link RepositoryEventFeatureUsage} with limited events.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class RepositoryFeatureUsageInMemory extends AbstractRepositoryFeatureUsage {
   
    /** serialVersionUID. */
    private static final long serialVersionUID = 2667121403242303018L;

    /** default retention. */
    private static final int DEFAULT_QUEUE_CAPACITY = 100000;

    /** current capacity. */
    private int queueCapacity = DEFAULT_QUEUE_CAPACITY;

    /** Event <YYYYMMDD> / <featureUID> -> <Event> list (only action CHECK_ON) */
    private Map<String, Map<String, EventSeries>> events = 
            new ConcurrentHashMap<String, Map<String, EventSeries>>();
 
    /**
     * Default constructor with default capacity to 100.000
     */
    public RepositoryFeatureUsageInMemory() {
        this(DEFAULT_QUEUE_CAPACITY);
    }
    
    /**
     * Constructor to tune capacity.
     * 
     * @param queueCapacity
     *            default queue capacity
     */
    public RepositoryFeatureUsageInMemory(int queueCapacity) {
        this.queueCapacity = queueCapacity;
    }
    
    /** {@inheritDoc} */
    @Override
    public Stream<String> findAllIds() {
        if (events ==  null) return null;
        return events.keySet().stream();
    }
    
    /** {@inheritDoc} */
    @Override
    public Stream<Event> findAll() {
       List < Event > listOfEvents = new ArrayList<>();
       events.values().stream()
           // Stream <  Collection < EventSeries > >
           .map(m -> m.values())
           // Stream <  Stream < EventSeries > >
           .flatMap(Collection::stream)
           // Collection < EventSeries >
           .collect(Collectors.toList())
           // Stream < EventSeries >
           .stream()
           // EventSeries
           .forEach(listOfEvents::addAll);
       return listOfEvents.stream();
    }

    private Optional < EventSeries > findEventSeries(String eventUid) {
        return events.values().stream()
            // Get Collection < EventSeries >
            .map(m -> m.values())
            // Get only EventSeries
            .flatMap(Collection::stream)
            // Match correct and find UID
            .filter(serie -> serie.contains(eventUid))
            // Stop as soon as one is found
            .findFirst();
    }
    
    /** {@inheritDoc} */
    @Override
    public Optional<Event> find(String id) {
       Optional < EventSeries > result = findEventSeries(id);
       return result.isPresent() ? result.get().getById(id) : Optional.empty();
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean exists(String id) {
        return find(id).isPresent();
    }


    /** {@inheritDoc} */
    @Override
    public void save(Iterable<Event> entities) {
        assertNotNull(entities);
        entities.forEach(evt -> {
            Optional < EventSeries > result = findEventSeries(evt.getUid());
        if (result.isPresent()) {
            result.get().removeIf(e -> evt.getUid().equals(e.getUid()));
            result.get().add(evt);
        } else {
            // Does not exist, create
            saveEvent(evt);
        }});
    }
    
    /** {@inheritDoc} */
    protected void update(Event entity) {
        // Does this event exist ? 
        
    }

    @Override
    public void delete(Iterable<String> entities) {
        assertNotNull(entities);
        entities.forEach(this::deleteEvent);
    }
    
    /** {@inheritDoc} */
    public void deleteEvent(String entityId) {
        Optional < EventSeries > result = findEventSeries(entityId);
        if (result.isPresent()) {
            result.get().removeIf(e -> entityId.equals(e.getUid()));
        }
    }

   
    
    private boolean match(Event e) {
        return (e!= null) && e.getScope().equals(Event.Scope.FEATURE.name())
                          && Event.Action.HIT.name().equalsIgnoreCase(e.getAction());
    }
    
    /** {@inheritDoc} */
    public void saveEvent(Event e) {
        if (match(e)) saveEvent(e, events);
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, HitCount> getHitCount(EventQueryDefinition query) {
        Map<String, HitCount> hitRatio = new TreeMap<String, HitCount>();
        for (Event event : search(query)) {
            if (!hitRatio.containsKey(event.getTargetUid())) {
                hitRatio.put(event.getTargetUid(), new HitCount());
             }
             hitRatio.get(event.getTargetUid()).inc();
        }
        return hitRatio;
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, HitCount> getSourceHitCount(EventQueryDefinition query) {
        Map<String, HitCount> hitRatio = new TreeMap<String, HitCount>();
        for (Event event : search(query)) {
            if (!hitRatio.containsKey(event.getSource())) {
                hitRatio.put(event.getSource(), new HitCount());
             }
             hitRatio.get(event.getSource()).inc();
        }
        return hitRatio;
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, HitCount> getHostHitCount(EventQueryDefinition query) {
        Map<String, HitCount> hitRatio = new TreeMap<String, HitCount>();
        for (Event event : search(query)) {
            if (!hitRatio.containsKey(event.getHostName())) {
                hitRatio.put(event.getHostName(), new HitCount());
             }
             hitRatio.get(event.getHostName()).inc();
        }
        return hitRatio;
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, HitCount> getUserHitCount(EventQueryDefinition query) {
        Map<String, HitCount> hitRatio = new TreeMap<String, HitCount>();
        for (Event event : search(query)) {
            String user = event.getOwner().orElse("anonymous");
            if (!hitRatio.containsKey(user)) {
                hitRatio.put(user, new HitCount());
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
        String uid = e.getTargetUid();
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
            if (events.containsKey(currentDay)) {
                for (Map.Entry<String, EventSeries> entry : events.get(currentDay).entrySet()) {
                    String currentFeatureName = entry.getKey();
                    // Filter feature names if required
                    Set < String > filteredFeatures = query.getNamesFilter();
                    if (filteredFeatures == null || filteredFeatures.isEmpty() || filteredFeatures.contains(currentFeatureName)) {
                        // Loop over events
                        for (Event evt : entry.getValue()) {
                            // Between bounds (keydate not enough)
                            if (evt.isInInterval(query.getFrom(), query.getTo())) {
                                // Create new serie if new feature Name
                                if (!tsc.getSeries().containsKey((currentFeatureName))) {
                                    tsc.createNewSerie(currentFeatureName);
                                }
                                // Match FeatureName
                                Serie < Map<String , HitCount > > serie = tsc.getSeries().get(currentFeatureName);
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
        return tsc;
    }
    
    /** {@inheritDoc} */
    @Override
    public void purge(EventQueryDefinition q) {
        Set<String> candidateDates = getCandidateDays(q.getFrom(), q.getTo());
        for (String currentDay : candidateDates) {
            if (events.containsKey(currentDay)) {
                Map<String, EventSeries> currentDayEvents = events.get(currentDay);
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
                    events.remove(currentDay);
                }
            }
        }
    }
    
    private void removeEventIfPresent(EventSeries es, Event evt) {
        Iterator < Event > iterEvt = es.iterator();
        while(iterEvt.hasNext()) {
            Event currentEvent = iterEvt.next();
            if (currentEvent.getUid().equalsIgnoreCase(evt.getUid())) {
                es.remove(currentEvent);
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public EventSeries search(EventQueryDefinition query) {
        EventSeries es = new EventSeries(1000000);
        // Dates are the keys of the storage map, compute list of keys and loop over them
        for (String currentDay : getCandidateDays(query.getFrom(), query.getTo())) {
            // There are some events with the current date
            if (events.containsKey(currentDay)) {
               Map<String, EventSeries> currentDayEvents = events.get(currentDay);
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
    public Event searchEventById(String uuid, String targetDate) {
        assertHasLength(targetDate);
        assertHasLength(uuid);
        Map < String, EventSeries > maOfFeaturesIsages= events.get(targetDate);
        if (maOfFeaturesIsages != null ) {
            for (EventSeries es : maOfFeaturesIsages.values()) {
                Event evt = getFromEventSeries(es, uuid);
                if (evt != null)  return evt;
            }
        }
        return null;
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
            if (evt.getUid().equalsIgnoreCase(uuid)) {
                return evt;
            }
        }
        return null;
    }

}
