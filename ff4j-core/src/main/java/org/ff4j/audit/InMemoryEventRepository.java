package org.ff4j.audit;

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

import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ConcurrentHashMap;

import org.ff4j.audit.graph.Curve;
import org.ff4j.audit.graph.PieGraph;
import org.ff4j.audit.graph.PieSector;
import org.ff4j.utils.Util;

/**
 * Implementation of in memory {@link EventRepository} with limited events.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class InMemoryEventRepository implements EventRepository {

    /** total hit count. */
    private static final String TITLE_PIE_HITCOUNT = "Total Hit Counts";
    
    /** default retention. */
    private static final int DEFAULT_QUEUE_CAPACITY = 10000;

    /** current capacity. */
    private int queueCapacity = DEFAULT_QUEUE_CAPACITY;

    /** Store for events. */
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
        if (myQueue.size() >= DEFAULT_QUEUE_CAPACITY) {
            myQueue.poll();
        }
        return myQueue.offer(e);
    }
    
    /** {@inheritDoc} */
    @Override
    public PieGraph getTotalHitsPie(long startTime, long endTime) {
        PieGraph pieGraph = new PieGraph("Total Hits Count");
        List < String > colors   = Util.getColorsGradient(mapOfEvents.size());
        List < String > features = new ArrayList<String>(mapOfEvents.keySet());
        for(int idx = 0; idx < mapOfEvents.size();idx++) {
            Queue< Event > qEvents = mapOfEvents.get(features.get(idx));
            int counter = 0;
            for (Event evt : qEvents) {
                if (evt.getTimestamp() > startTime && evt.getTimestamp() < endTime) {
                    if (EventType.HIT_FLIPPED.equals(evt.getType())) {
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
    public PieGraph getFeatureHitsPie(String featureId, long startTime, long endTime) {
        List < String > colors   = Util.getColorsGradient(4);
        Queue< Event > qEvents = mapOfEvents.get(featureId);
        PieGraph pieGraph = new PieGraph("Hits Count for " + featureId);
        int nbEnable = 0;
        int nbDisable = 0;
        int nbFlip = 0;
        int notFlip = 0;
        if (null != qEvents) {
            for (Event evt : qEvents) {
                if (evt.getTimestamp() > startTime && evt.getTimestamp() < endTime) {
                    switch (evt.getType()) {
                        case HIT_FLIPPED:
                            nbFlip++;
                        break;
                        case HIT_NOT_FLIPPED:
                            notFlip++;
                        break;
                        case ENABLE:
                            nbEnable++;
                        break;
                        case DISABLE:
                            nbDisable++;
                        default:
                        break;
                    }
                }
            }
        }
        pieGraph.getSectors().add(new PieSector("ENABLE", nbEnable, colors.get(0)));
        pieGraph.getSectors().add(new PieSector("DISABLE", nbDisable, colors.get(1)));
        pieGraph.getSectors().add(new PieSector("FLIP", nbFlip, colors.get(2)));
        pieGraph.getSectors().add(new PieSector("NOT_FLIP", notFlip, colors.get(3)));
        return pieGraph;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Curve> getHitCurves(Set<String> featNameSet, long startTime, long endTime, int nbslot) {

        // (0) - Initialization of curves
        Map<String, Curve> maps = new HashMap<String, Curve>();

        // (1) Loop over expected queues
        for (String name : featNameSet) {
            Queue<Event> myQueue = mapOfEvents.get(name);
            if (myQueue != null) {
                // Current curve
                Curve curve = new Curve(name, startTime, endTime, nbslot);
                for (Iterator<Event> itEvt = myQueue.iterator(); itEvt.hasNext();) {
                    Event evt = itEvt.next();
                    long t = evt.getTimestamp();
                    // Is in target window
                    if (startTime < t && t < endTime) {
                        if (EventType.HIT_FLIPPED.equals(evt.getType())) {
                            long slot = (t - startTime) / curve.getInterval();
                            curve.incrCount((int) slot);
                        }
                    }
                }
                maps.put(name, curve);
            }
        }
        return maps;
    }

    /** {@inheritDoc} */
    @Override
    public Curve getTotalHitsCurve(long startTime, long endTime, int nbRecord) {
        Curve curve = new Curve(TITLE_PIE_HITCOUNT, startTime, endTime, nbRecord);
        for (String qName : mapOfEvents.keySet()) {
            Queue< Event > qEvents = mapOfEvents.get(qName);
            for (Event evt : qEvents) {
                long t = evt.getTimestamp();
                if (startTime < t && t < endTime) {
                    long slot = (t - startTime) / curve.getInterval();
                    curve.incrCount((int) slot);
                } 
            }
        }
        return curve;
    }
    
    /** {@inheritDoc} */
    @Override
    public Curve getFeatureHitsCurve(String featureName, long startTime, long endTime, int nbRecord) {
        return getHitCurves(Util.hashSet(featureName), startTime, endTime, nbRecord).get(featureName);
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> getCurveList() {
        return mapOfEvents.keySet();
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("{");
        sb.append("\"type\":\"" + this.getClass().getCanonicalName() + "\"");
        
        // Create Today PIE
        Calendar c = Calendar.getInstance();
        c.set(Calendar.HOUR_OF_DAY, 0);
        c.set(Calendar.MINUTE, 0);
        c.set(Calendar.SECOND, 0);
        PieGraph pie = getTotalHitsPie(c.getTimeInMillis(), System.currentTimeMillis());
        sb.append(",\"todayHitsPie\": " + pie);
        
        // Create today curve
        Curve curve = getTotalHitsCurve(c.getTimeInMillis(), System.currentTimeMillis(), 100);
        sb.append(",\"todayHitsCurve\": " + curve);
        
        int total = 0;
        for(PieSector sector : pie.getSectors()) {
            total += sector.getValue();
        }
        sb.append(",\"todayHitTotal\":" + total);
        sb.append("}");
        return sb.toString();
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

    

   

   

}
