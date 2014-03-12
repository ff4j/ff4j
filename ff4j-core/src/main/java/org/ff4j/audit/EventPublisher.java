package org.ff4j.audit;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * Default implementation of repository.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class EventPublisher {

    /** default pool size. */
    private static final int DEFAULT_POOL_SIZE = 3;

    /** Executor for item writer. */
    private ExecutorService executor = null;

    /** Repository to save events. */
    private EventRepository repository = new EventRepositoryVM();

    /**
     * Default constructor.
     */
    public EventPublisher() {
        this(DEFAULT_POOL_SIZE);
    }

    /**
     * Size of thread pool.
     * 
     * @param threadCount
     */
    public EventPublisher(int threadCount) {
        executor = Executors.newFixedThreadPool(threadCount);
    }

    /** {@inheritDoc} */
    public void publish(Event e) {
        executor.submit(new EventWorker(e, repository));
    }

    /**
     * Setter accessor for attribute 'repository'.
     * 
     * @param repository
     *            new value for 'repository '
     */
    public void setRepository(EventRepository repository) {
        this.repository = repository;
    }

    /**
     * Getter accessor for attribute 'repository'.
     * 
     * @return current value of 'repository'
     */
    public EventRepository getRepository() {
        return repository;
    }

}
