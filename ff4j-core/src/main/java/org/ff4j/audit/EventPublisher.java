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
import java.util.concurrent.atomic.AtomicInteger;

import org.ff4j.audit.repository.EventRepository;
import org.ff4j.audit.repository.InMemoryEventRepository;

/**
 * Default implementation of repository.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class EventPublisher {
    
    /** DEFAULT. */
    public static final int DEFAULT_QUEUE_CAPACITY = 100;
    
    /** DEFAULT. */
    public static final int DEFAULT_POOL_SIZE = 4;
    
    /** 2s to save the event other wize skip. */
    public static long timeout = 2000L;
    
    /** queueing incoming events. */
    private final BlockingQueue<Runnable> queue;
    
    /** Executor for item writer. */
    private ExecutorService executor;

    /** Repository to save events. */
    private EventRepository repository;

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
        // Initializing queue
        queue = new ArrayBlockingQueue<Runnable>(queueCapacity);
        
        // Executor with worker to process threads
        RejectedExecutionHandler rej = (new RejectedExecutionHandler() {
            @Override
            public void rejectedExecution(Runnable r, ThreadPoolExecutor executor) {
                try {
                    Thread.sleep(1000);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                // try once again
                executor.execute(r);
            }
        });
        
         class CustomThreadFactory implements ThreadFactory {
            final AtomicInteger poolNumber = new AtomicInteger(1);
            final ThreadGroup group;
            final AtomicInteger threadNumber = new AtomicInteger(1);
            final String namePrefix;
     
            CustomThreadFactory() {
                SecurityManager s = System.getSecurityManager();
                group = (s != null)? s.getThreadGroup() : Thread.currentThread().getThreadGroup();
                namePrefix = "ff4j-monitoring-pool-" + poolNumber.getAndIncrement() +  "-thread-";
            }
     
            public Thread newThread(Runnable r) {
                Thread t = new Thread(group, r, namePrefix + threadNumber.getAndIncrement(), 0);
                if (t.isDaemon())
                    t.setDaemon(false);
                if (t.getPriority() != Thread.NORM_PRIORITY)
                    t.setPriority(Thread.NORM_PRIORITY);
                return t;
            }
        };
      
        executor = new ThreadPoolExecutor(poolSize, poolSize, 0L, TimeUnit.MILLISECONDS, queue, new CustomThreadFactory() , rej);
       
        // Override repository
        repository = er;
    }

    /**
     * Publish event to repository
     * 
     * @param e
     *            event.
     */
    public void publish(Event e) {
        final Future<Boolean> check = executor.submit(new EventWorker(e, repository));
        try {
            if (timeout != 0) {
                check.get(timeout, TimeUnit.MILLISECONDS);
            }
        } catch (Exception e1) {
            e1.printStackTrace();
            System.err.println("Cannot push event into monitoring");
        }
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
        Event evt = new Event(featureName, EventType.FEATURE_CHECK_ON);
        if (!flipped) {
            evt.setType(EventType.FEATURE_CHECK_OFF);
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
