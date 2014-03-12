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
import java.util.Iterator;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.ArrayBlockingQueue;

/**
 * Implementation of in memory {@link EventRepository} with limited events.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class EventRepositoryVM implements EventRepository {

    /** Store for events. */
    private static Queue<Event> events = null;

    /** default retention. */
    private static final int DEFAULT_QUEUE_CAPACITY = 100000;

    /**
     * Default constructor with default capacity to 100.000
     */
    public EventRepositoryVM() {
        this(DEFAULT_QUEUE_CAPACITY);
    }

    /**
     * Constructor to tune capacity.
     * 
     * @param queueCapacity
     *            default queue capacity
     */
    public EventRepositoryVM(int queueCapacity) {
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

}
