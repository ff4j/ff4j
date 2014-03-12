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
