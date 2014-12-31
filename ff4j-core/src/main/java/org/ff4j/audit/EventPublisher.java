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

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.ff4j.audit.repository.EventRepository;
import org.ff4j.audit.repository.InMemoryEventRepository;

/**
 * Default implementation of repository.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class EventPublisher {

    /** default pool size. */
    private static final int DEFAULT_POOL_SIZE = 3;

    /** Executor for item writer. */
    private ExecutorService executor = Executors.newFixedThreadPool(DEFAULT_POOL_SIZE);

    /** Repository to save events. */
    private EventRepository repository = new InMemoryEventRepository();

    /**
     * Default constructor.
     */
    public EventPublisher() {
    }

    /**
     * Size of thread pool.
     * 
     * @param threadCount
     */
    public EventPublisher(int threadCount) {
        executor = Executors.newFixedThreadPool(threadCount);
    }

    /**
     * Constructor with repository
     * 
     * @param er
     *            target repository
     */
    public EventPublisher(EventRepository er) {
        repository = er;
    }

    /**
     * Size of thread pool.
     * 
     * @param threadCount
     *            thread pool size
     * @param er
     *            target repository
     */
    public EventPublisher(int threadCount, EventRepository er) {
        executor = Executors.newFixedThreadPool(threadCount);
        repository = er;
    }

    /**
     * Publish event to repository
     * 
     * @param e
     *            event.
     */
    public void publish(Event e) {
        executor.submit(new EventWorker(e, repository));
    }

    /**
     * Publish event to repository.
     * 
     * @param featureName
     *            target feature name
     * @param type
     *            event type
     */
    public void publish(String featureName, EventType type) {
        publish(new Event(featureName, type));
    }

    /**
     * Paramterized constructor.
     * 
     * @param featureName
     *            target feature name
     * @param flipped
     *            if flipped
     */
    public void publish(String featureName, boolean flipped) {
        Event evt = new Event(featureName, EventType.HIT_FLIPPED);
        if (!flipped) {
            evt.setType(EventType.HIT_NOT_FLIPPED);
        }
        publish(evt);
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
