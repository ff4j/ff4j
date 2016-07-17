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


import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
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
    
    /**
     * Create slots and initiate data structure.
     *
     * @param startTime
     *      period start date
     * @param endTime
     *      period end date
     * @param units
     *      current units
     * @return
     * 
     */
    private TimeSeriesChart initSlots(EventQueryDefinition query, TimeUnit units) {
        TimeSeriesChart tsc = new TimeSeriesChart();
        long slotWitdh = 0;
        switch (units) {
            case MINUTES:
                slotWitdh = 1000 * 60;
                tsc.setSdf(new SimpleDateFormat("yyyyMMdd-HH:mm"));
            break;
            case HOURS:
                slotWitdh = 1000 * 60 * 60;
                tsc.setSdf(new SimpleDateFormat("yyyyMMdd-HH"));
            break;
            case DAYS:
                slotWitdh = 1000 * 60 * 60 * 24;
                tsc.setSdf(new SimpleDateFormat("yyyyMMdd"));
            break;
            default:
                slotWitdh = 1000;
                tsc.setSdf(new SimpleDateFormat("yyyyMMdd-HH:mm:ss"));
            break;
        }
        // Create slots for the timeSeries base ones
        int nbslot = new Long(1 + (query.getTo() - query.getFrom()) / slotWitdh).intValue();
        for (int i = 0; i < nbslot; i++) {
            tsc.getTimeSlots().add(tsc.getSdf().format(new Date(query.getFrom() + slotWitdh * i)));
        }
        return tsc;
    }
    
    /** {@inheritDoc} */
    @Override
    public TimeSeriesChart getFeatureUsageHistory(EventQueryDefinition query, TimeUnit units) {
        TimeSeriesChart tsc = initSlots(query, units);

        for (String currentDay : getCandidateDays(query.getFrom(), query.getTo())) {
            // There are some event this day
            if (featureUsageEvents.containsKey(currentDay)) {
                for (Map.Entry<String, EventSeries> entry : featureUsageEvents.get(currentDay).entrySet()) {
                    // Filter feature names if required
                    Set < String > filteredFeatures = query.getNamesFilter();
                    if (filteredFeatures == null || filteredFeatures.isEmpty() || filteredFeatures.contains(entry.getKey())) {
                        // Loop over events
                        for (Event evt : entry.getValue()) {
                            // Between bounds (keydate not enough)
                            if (isEventInInterval(evt, query.getFrom(), query.getTo())) {
                                // Create new serie if new feature Name
                                if (!tsc.getSeries().containsKey((entry.getKey()))) {
                                    tsc.createNewSerie(entry.getKey());
                                }

                                // Get correct slot
                                String evtKeyDate = tsc.getSdf().format(new Date(evt.getTimestamp()));

                                // match featureName, match slot, increment
                                tsc.getSeries().get(entry.getKey()).getValue().get(evtKeyDate).inc();
                            }
                        }
                    }
                }
            }
        }
        
        // Recolor series
        List < String > colors = Util.getColorsGradient(tsc.getSeries().size());
        int idxColor = 0;
        for (Map.Entry<String, Serie<Map<String, MutableHitCount>>> serie : tsc.getSeries().entrySet()) {
            serie.getValue().setColor(colors.get(idxColor));
            idxColor++;
        }
        return tsc;
    }

    /** {@inheritDoc} */
    @Override
    public TimeSeriesChart getAverageResponseTime(EventQueryDefinition query, TimeUnit tu) {
        // Create the chart (empty slots)
        TimeSeriesChart tsc = initSlots(query, tu);

        // Temporary element featureName -> slotKey -> List points
        Map < String , Map < String, EventSeries > > mapOfValues = new HashMap<String, Map<String, EventSeries > >();
       
        for (String currentDay : getCandidateDays(query.getFrom(), query.getTo())) {
            if (featureUsageEvents.containsKey(currentDay)) {
                for (Map.Entry<String, EventSeries> entry : featureUsageEvents.get(currentDay).entrySet()) {
                    for (Event evt : entry.getValue()) {
                        if (query.match(evt)) {
                            // Get correct slot
                            if (!mapOfValues.containsKey(entry.getKey()) ) {
                                mapOfValues.put(entry.getKey(), new HashMap<String, EventSeries>()); 
                            }
                            String slotKey = tsc.getSdf().format(new Date(evt.getTimestamp()));
                            if (!mapOfValues.get(entry.getKey()).containsKey(slotKey) ) {
                                mapOfValues.get(entry.getKey()).put(slotKey, new EventSeries());
                            }
                            mapOfValues.get(entry.getKey()).get(slotKey).add(evt);
                        }
                    }
                }
            }
        }
        
        // Calculate AVG
        for(String featureName : mapOfValues.keySet()) {
            if (!tsc.getSeries().containsKey(featureName)) {
                tsc.createNewSerie(featureName);
            }
            for(Map.Entry<String, EventSeries > entryValue : mapOfValues.get(featureName).entrySet()) {
                int average = new Double(entryValue.getValue().getAverageDuration()).intValue();
                tsc.getSeries().get(featureName).getValue().put(entryValue.getKey(), new MutableHitCount(average));
            }
        }
        
        // Update Colors
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
                                currentDayEvents.remove(evt);
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

    /** {@inheritDoc} */
    @Override
    public EventSeries searchFeatureUsageEvents(EventQueryDefinition query) {
        EventSeries es = new EventSeries(1000000);
        for (String currentDay : getCandidateDays(query.getFrom(), query.getTo())) {
            if (featureUsageEvents.containsKey(currentDay)) {
                Map<String, EventSeries> currentDayEvents = featureUsageEvents.get(currentDay);
                for (String currentFeature : currentDayEvents.keySet()) {
                    if (query.matchName(currentFeature)) {
                        Iterator<Event> iterEvents = currentDayEvents.get(currentFeature).iterator();
                        // Loop over events
                        while (iterEvents.hasNext()) {
                            Event evt = iterEvents.next();
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
   
}
