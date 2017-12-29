package org.ff4j.cache;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import java.io.Serializable;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;

import org.ff4j.FF4jEntity;

/**
 * Polling target store and populate relevant cache.
 *
 * @author Cedrick LUNVEN  (@clunven)
 * @author Andre Blaszczyk (@AndrBLASZCZYK)
 *
 * @param <WORKER>
 */
public class CachePollingScheduler< V extends FF4jEntity<?> > implements Serializable {
    
    /** Serial. */
    private static final long serialVersionUID = -1198719730422859724L;

    /** polling delay. */
    protected long pollingDelay = 10000;
    
    /** initial delay at start. */
    protected long initialDelay = 0;
    
    /** Scheduler for the worker. */
    protected ScheduledExecutorService executor;

    /** Current runnable. */
    protected CacheWorker<V> worker;
    
    public void initExecutor(String threadName) {
        executor = Executors.newScheduledThreadPool(1, new ThreadFactory() {
            @Override
            public Thread newThread(Runnable r) {
                Thread t = new Thread(r, threadName);
                t.setDaemon(true);
                return t;
            }
        });
    }
    
    /**
     * Start polling with a polling
     */
    public void start(long delay) {
        this.pollingDelay = delay;
        start();
    }
    
    /**
     * Start polling.
     */
    public void start() {
        executor.scheduleWithFixedDelay(worker, initialDelay, pollingDelay, TimeUnit.MILLISECONDS);
    }
    
    /** Stop Polling. */
    public void stop() {
        if (executor != null) {
            executor.shutdown();
            executor = null;
        }
    }

    /**
     * Getter accessor for attribute 'pollingDelay'.
     *
     * @return
     *       current value of 'pollingDelay'
     */
    public long getPollingDelay() {
        return pollingDelay;
    }

    /**
     * Setter accessor for attribute 'pollingDelay'.
     * @param pollingDelay
     *      new value for 'pollingDelay '
     */
    public void setPollingDelay(long pollingDelay) {
        this.pollingDelay = pollingDelay;
    }

    /**
     * Getter accessor for attribute 'initialDelay'.
     *
     * @return
     *       current value of 'initialDelay'
     */
    public long getInitialDelay() {
        return initialDelay;
    }

    /**
     * Setter accessor for attribute 'initialDelay'.
     * @param initialDelay
     *      new value for 'initialDelay '
     */
    public void setInitialDelay(long initialDelay) {
        this.initialDelay = initialDelay;
    }

}
