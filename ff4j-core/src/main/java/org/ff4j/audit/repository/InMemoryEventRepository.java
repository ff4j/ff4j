package org.ff4j.audit.repository;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ConcurrentHashMap;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventType;
import org.ff4j.audit.graph.BarChart;
import org.ff4j.audit.graph.BarSeries;
import org.ff4j.audit.graph.PieChart;
import org.ff4j.audit.graph.PieSector;
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

    /** Store : < FeatureName |  QueueOfEvents > */
    private final Map<String, Queue<Event>> mapOfEvents = new ConcurrentHashMap<String, Queue<Event>>();

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
        if (!mapOfEvents.containsKey(e.getFeatureName())) {
            mapOfEvents.put(e.getFeatureName(), new ArrayBlockingQueue<Event>(queueCapacity));
        }
        Queue<Event> myQueue = mapOfEvents.get(e.getFeatureName());
        if (myQueue.size() >= queueCapacity) {
            myQueue.poll();
        }
        return myQueue.offer(e);
    }
    
    /** {@inheritDoc} */
    @Override
    public PieChart getHitsPieChart(long startTime, long endTime) {
        PieChart pieGraph = new PieChart(TITLE_PIE_HITCOUNT);
        List < String > colors   = Util.getColorsGradient(mapOfEvents.size());
        List < String > features = new ArrayList<String>(mapOfEvents.keySet());
        for(int idx = 0; idx < mapOfEvents.size();idx++) {
            Queue< Event > qEvents = mapOfEvents.get(features.get(idx));
            int counter = 0;
            for (Event evt : qEvents) {
                if (evt.getTimestamp() > startTime && evt.getTimestamp() < endTime) {
                    if (EventType.FEATURE_CHECK_ON.equals(evt.getType())) {
                        counter++;
                    }
                }
            }
            pieGraph.getSectors().add(new PieSector(features.get(idx), counter, colors.get(idx)));
        }
        return pieGraph;
    }
    
    /** {@inheritDoc} */
    @Override
    public PieChart getFeatureHitsPie(String featureId, long startTime, long endTime) {
        List < String > colors   = Util.getColorsGradient(4);
        Queue< Event > qEvents = mapOfEvents.get(featureId);
        PieChart pieGraph = new PieChart("Hits Count for " + featureId);
        int nbEnable = 0;
        int nbDisable = 0;
        int nbFlip = 0;
        int notFlip = 0;
        if (null != qEvents) {
            for (Event evt : qEvents) {
                if (evt.getTimestamp() > startTime && evt.getTimestamp() < endTime) {
                    switch (evt.getType()) {
                        case FEATURE_CHECK_ON:
                            nbFlip++;
                        break;
                        case FEATURE_CHECK_OFF:
                            notFlip++;
                        break;
                        case ENABLE_FEATURE:
                            nbEnable++;
                        break;
                        case DISABLE_FEATURE:
                            nbDisable++;
                        default:
                        break;
                    }
                }
            }
        }
        if (nbEnable > 0) {
            pieGraph.getSectors().add(
                    new PieSector(EventType.ENABLE_FEATURE.toString(), nbEnable, colors.get(0)));
        }
        if (nbDisable > 0) {
            pieGraph.getSectors().add(new PieSector(
                    EventType.DISABLE_FEATURE.toString(), nbDisable, colors.get(1)));
        }
        if (nbFlip > 0) {
            pieGraph.getSectors().add(new PieSector(
                    EventType.FEATURE_CHECK_ON.toString(), nbFlip, colors.get(2)));
        }
        if (notFlip > 0) {
            pieGraph.getSectors().add(new PieSector(
                    EventType.FEATURE_CHECK_OFF.toString(), notFlip, colors.get(3)));
        }
        return pieGraph;
    }
    
    /** {@inheritDoc} */
    @Override
    public BarChart getHitsBarChart(Set<String> featNameSet, long startTime, long endTime, int nbslot) {
        // Initialization of chart
        
        // Build Labels
        long slotWitdh = (endTime - startTime) / nbslot;
        SimpleDateFormat sdf = new SimpleDateFormat("HH:mm");
        List <String> labels = new ArrayList<String>();
        for (int i = 0; i < nbslot; i++) {
            labels.add(sdf.format(new Date(startTime + slotWitdh * i)));
        }
        
        // Build SeriesNames
        BarChart barChart = new BarChart(TITLE_BARCHAR_HIT, labels, new ArrayList<String>(featNameSet));
        for (String name : featNameSet) {
          // Retrieve events for target feature
          Queue<Event> myQueue = mapOfEvents.get(name);
          // Create series for this feature (even if not present)
          BarSeries currentSeries = barChart.getSeries().get(name);
          if (myQueue != null) {
             for (Iterator<Event> itEvt = myQueue.iterator(); itEvt.hasNext();) {
                 Event evt = itEvt.next();
                 long t = evt.getTimestamp();
                 // Filter event in the slot and type flipped (= used)
                 if (startTime < t && t < endTime && EventType.FEATURE_CHECK_ON.equals(evt.getType())) {
                     currentSeries.incrCount((int) ((t - startTime) / slotWitdh));
                 }
             }
          }
        }
        return barChart;
    }

    /** {@inheritDoc} */
    @Override
    public int getTotalEventCount() {
        Set<String> queues = mapOfEvents.keySet();
        int total = 0;
        for (String queue : queues) {
            total += mapOfEvents.get(queue).size();
        }
        return total;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> getFeatureNames() {
        return  mapOfEvents.keySet();
    }

}
