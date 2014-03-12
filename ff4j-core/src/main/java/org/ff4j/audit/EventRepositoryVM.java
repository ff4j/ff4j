package org.ff4j.audit;

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
