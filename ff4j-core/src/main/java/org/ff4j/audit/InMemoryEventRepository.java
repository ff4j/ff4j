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

import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ConcurrentHashMap;

import org.ff4j.audit.graph.Curve;

/**
 * Implementation of in memory {@link EventRepository} with limited events.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class InMemoryEventRepository implements EventRepository {

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
    public Map<String, Curve> getHitCurves(Set<String> featNameSet, long interval, long startTime, long endTime) {

        // (0) - Initialization of curves
        Map<String, Curve> maps = new HashMap<String, Curve>();

        // (1) Loop over expected queues
        for (String name : featNameSet) {
            Queue<Event> myQueue = mapOfEvents.get(name);
            if (myQueue != null) {
                // Current curve
                Curve curve = new Curve(name, startTime, endTime, interval);
                for (Iterator<Event> itEvt = myQueue.iterator(); itEvt.hasNext();) {
                    long t = itEvt.next().getTimestamp();
                    // Is in target window
                    if (startTime < t && t < endTime) {
                        long slot = ((t - startTime) / interval) + 1;
                        curve.incrCount((int) slot);
                    }
                }
                maps.put(name, curve);
            }
        }
        return maps;
    }

    /** {@inheritDoc} */
    @Override
    public Curve getHitCurve(String featureName, long interval, long startTime, long endTime) {
        Set<String> singleElementSet = new HashSet<String>(Arrays.asList(featureName));
        return getHitCurves(singleElementSet, interval, startTime, endTime).get(featureName);
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
        // Today
        Calendar c = Calendar.getInstance();
        c.set(Calendar.HOUR_OF_DAY, 0);
        c.set(Calendar.MINUTE, 0);
        c.set(Calendar.SECOND, 0);
        sb.append(",\"todayHits\": ");
        /*Map<String, Integer> hitToday = getHitsCount(c.getTimeInMillis(), System.currentTimeMillis());
        boolean first = true;
        int totalHit = 0;
        for (String featureName : hitToday.keySet()) {
            if (!first) {
                sb.append(", ");
            }
            int tmpHit = hitToday.get(featureName);
            sb.append("\"" + featureName + "\":" + tmpHit);
            totalHit += tmpHit;
            first = false;
        }*/
        sb.append("0");
        sb.append("}");
        sb.append(",\"todayTotalHit\":" + 0);
        sb.append("");
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

    /** {@inheritDoc} */
    @Override
    public Map<String, Integer> getHitsCount(long startTime, long endTime) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Curve getTotalHitCurve(long interval, long startTime, long endTime) {
        return null;
    }


}
