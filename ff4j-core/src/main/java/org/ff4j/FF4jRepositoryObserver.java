package org.ff4j;

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

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.function.Consumer;

/**
 * Common Observable pattern to be reused in AuditTrail and FeatureUsage Tracking.
 *
 * @author Cedrick LUNVEN  (@clunven)
 *
 * @param <LISTENER>
 *      target listener type
 */
public class FF4jRepositoryObserver < LISTENER > {
    
    /** My list of listener to test. */
    protected Map < String, LISTENER > listeners = new HashMap<>();
    
    /** Optimize Thread-Safe. */
    private final ReadWriteLock readWriteLock = new ReentrantReadWriteLock();
    
    /** Read element. */
    protected final Lock readLock = readWriteLock.readLock();
    
    /** Write element. */
    protected final Lock writeLock = readWriteLock.writeLock();
    
    /**
     * Evaluate if a listener already exist.
     *
     * @param uid
     *      current listener UID
     * @return
     *      existence of listener
     */
    public boolean isExistListener(String uid) {
        return listeners.containsKey(uid);
    }
    
    /**
     * Access Listener (if exist)
     *
     * @param uid
     *      current listener UID
     * @return
     *      expected listener if exists
     */
    public Optional<LISTENER> getListener(String uid) {
        return Optional.ofNullable(listeners.get(uid));
    }
    
    /**
     * Register a listener in my observable strategy.
     *
     * @param listener
     *      
     * @return
     */
    public void registerListener(String name, LISTENER listener) {
        try {
            this.writeLock.lock();
            this.listeners.put(name, listener);
        }
        finally {
            this.writeLock.unlock();
        }
    }
    
    public void unregisterListener(String name) {
        try {
            this.writeLock.lock();
            this.listeners.remove(name);
        }
        finally {
            this.writeLock.unlock();
        }
    }
    
    /**
     * Execute target algo for each listener.
     *
     * @param lambda
     *      target lambda
     */
    public void notify(Consumer<? super LISTENER> lambda) {
        this.listeners.values().forEach(listener -> {
            CompletableFuture.runAsync((Runnable) (() -> lambda.accept(listener)));
        });
    }
    
}
