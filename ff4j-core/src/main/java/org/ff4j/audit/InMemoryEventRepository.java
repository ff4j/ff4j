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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;

import org.ff4j.audit.graph.Curve;

/**
 * Implementation of in memory {@link EventRepository} with limited events.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class InMemoryEventRepository implements EventRepository {

    /** Store for events. */
    private static Queue<Event> events = null;

    /** default retention. */
    private static final int DEFAULT_QUEUE_CAPACITY = 10000;

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
        events = new ArrayBlockingQueue<Event>(queueCapacity);
    }

    /** {@inheritDoc} */
    @Override
    public boolean saveEvent(Event e) {
        if (events.size() >= DEFAULT_QUEUE_CAPACITY) {
            events.poll();
        }
        events.offer(e);
        return true;
    }

    /** {@inheritDoc} */
    @Override
    public List<Event> getAllEvents() {
        List<Event> le = new ArrayList<Event>();
        for (Iterator<Event> itEvt = events.iterator(); itEvt.hasNext();) {
            le.add(itEvt.next());
        }
        return le;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Curve> getHitCurves(Set<String> featNameSet, long interval, long startTime, long endTime) {
        // (0) - Initialization of curves
        Map<String, Curve> maps = new HashMap<String, Curve>(featNameSet.size());
        for (String featureName : featNameSet) {
            maps.put(featureName, new Curve(featureName, startTime, endTime, interval));
        }

        // (1) - Loop on events to filters and calculate hit counts.
        for (Iterator<Event> itEvt = events.iterator(); itEvt.hasNext();) {
            Event ce = itEvt.next();
            // This event is in target windows and related to correct curve
            if (featNameSet.contains(ce.getFeatureName()) && (startTime < ce.getTimestamp()) && (ce.getTimestamp() < endTime)) {
                long slot = ((ce.getTimestamp() - startTime) / interval) + 1;
                maps.get(ce.getFeatureName()).incrCount((int) slot);
            }
        }

        return maps;
    }

    /** {@inheritDoc} */
    @Override
    public Curve getHitCurve(String featureName, long interval, long startTime, long endTime) {
        Set < String > singleElementSet = new HashSet<String>();
        singleElementSet.add(featureName);
        return getHitCurves(singleElementSet, interval, startTime, endTime).get(featureName);
    }
}
