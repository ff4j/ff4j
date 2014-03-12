package org.ff4j.audit;

import java.util.concurrent.Callable;

/**
 * Worker to save {@link Event} into {@link EventRepository} asynchronously.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class EventWorker implements Callable<Boolean> {

    /** Target event to insert. */
    private Event event = null;

    /** Repository to store event. */
    private EventRepository eventRepository = null;

    /** retry count if not available. */
    private static final int MAX_RETRY = 3;

    /** retry delay. */
    private static final long RETRY_DELAY = 500L;

    /**
     * Worker constructor.
     * 
     * @param e
     *            event
     * @param repo
     *            event repository to store events
     */
    public EventWorker(Event e, EventRepository repo) {
        this.event = e;
        this.eventRepository = repo;
    }

    /** {@inheritDoc} */
    @Override
    public Boolean call() throws Exception {
        boolean ok = false;
        int retryCount = 0;
        while (!ok && retryCount < MAX_RETRY) {
            ok = eventRepository.saveEvent(event);
            if (!ok) {
                retryCount++;
                Thread.sleep(RETRY_DELAY);
            }
        }
        return ok;
    }

}
