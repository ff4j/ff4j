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

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.ff4j.audit.repository.EventRepository;
import org.ff4j.audit.repository.InMemoryEventRepository;

/**
 * Default implementation of repository.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class EventPublisher {
   
    /** DEFAULT. */
    public static final int DEFAULT_QUEUE_CAPACITY = 100;
    
    /** DEFAULT. */
    public static final int DEFAULT_POOL_SIZE = 4;
    
    /** 2s to save the event other wize skip. */
    public static final long timeout = 2000L;
    
    /** Executor for item writer. */
    private ExecutorService executor;

    /** Repository to save events. */
    private EventRepository repository;

    /** the amount of time to wait after submitting for the task to complete. */
    private final long submitTimeout;

    /** flag to shiutdown executor on failure. */
    private final boolean shutdownExecutor;

    /**
     * Default constructor.
     */
    public EventPublisher() {
        this(DEFAULT_QUEUE_CAPACITY, DEFAULT_POOL_SIZE, new InMemoryEventRepository());
    }
    
    /**
     * Default constructor.
     */
    public EventPublisher(EventRepository er) {
        this(DEFAULT_QUEUE_CAPACITY, DEFAULT_POOL_SIZE, er);
    }
        
    /**
     * Default constructor.
     */
    public EventPublisher(int queueCapacity, int poolSize, EventRepository er) {
        this(queueCapacity, poolSize, er, timeout);
    }

    /**
     * Default constructor.
     */
    public EventPublisher(int queueCapacity, int poolSize, EventRepository er, long submitTimeout) {
        // Initializing queue
        final BlockingQueue<Runnable> queue = new ArrayBlockingQueue<Runnable>(queueCapacity);
        // Executor with worker to process threads
        RejectedExecutionHandler rej = new EventRejectedExecutionHandler();
        ThreadFactory tFactorty = new PublisherThreadFactory();
        this.executor = 
                new ThreadPoolExecutor(poolSize, poolSize, 0L, TimeUnit.MILLISECONDS, queue, tFactorty, rej);
        // Override repository
        this.repository = er;
        this.submitTimeout = submitTimeout;
        this.shutdownExecutor = true;
    }

    /**
     * @param er the event repository to use
     * @param executorService the executor service
     */
    public EventPublisher(EventRepository er, ExecutorService executorService) {
        this(er, executorService, timeout);
    }

    /**
     * @param er the event repository to use
     * @param executorService the executor service
     * @param submitTimeout
     */
    public EventPublisher(EventRepository er, ExecutorService executorService, long submitTimeout) {
        repository = er;
        executor = executorService;
        this.submitTimeout = submitTimeout;
        this.shutdownExecutor = false;
    }

    /**
     * Publish event to repository
     * 
     * @param e
     *            event.
     */
    public void publish(Event e) {
        try {
            EventWorker ew = new EventWorker(e, repository);
            final Future<Boolean> check = executor.submit(ew);
            check.get(submitTimeout, TimeUnit.MILLISECONDS);
        } catch (Exception e1) {
            // Do not propagate error, it's monitoring (aside business logic)
            //System.err.println("Cannot publish event " + e1.getMessage());
            //e1.printStackTrace();
        }
    }

    /**
     * Stops the event publisher. If we started an executor service, it will
     * be shutdown here.
     */
    public void stop() {
        if (this.shutdownExecutor) {
            this.executor.shutdownNow();
        }
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
