package org.ff4j.cache;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 Ff4J
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

/**
 * Cache entry with object and inserted Date.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public final class InMemoryCacheEntry<T> implements Serializable {

    /** serial. */
    private static final long serialVersionUID = -1444331517339058103L;
    
    /** Default TTL is one hour. */
    public static final long DEFAULT_TTL = 3600L;

    /** externalized as constant. */
    public static final long TO_MILLIS = 1000L;

    /** TTL of this entry. */
    private long timeToLive = DEFAULT_TTL;
    
    /** Insertion date, allow to compute time-to-live. */
    private final long insertedDate;

    /** Current entry to cache. */
    private final T entry;

    /**
     * Parameterized contructor with target cached object.
     * 
     * @param entry
     *            cached object
     */
    public InMemoryCacheEntry(T entry) {
        this.entry = entry;
        this.insertedDate = System.currentTimeMillis();
    }
    
    /**
     * Parameterized contructor with target cached object.
     * 
     * @param entry
     *            cached object
     */
    public InMemoryCacheEntry(T entry, long timeToLive) {
        this.entry = entry;
        this.insertedDate = System.currentTimeMillis();
        this.timeToLive = timeToLive;
    }
    
    /**
     * Compute the timeout property.
     *
     * @return
     *      time to live
     */
    public boolean hasReachTimeToLive() {
        return (System.currentTimeMillis() - getInsertedDate()) >= (TO_MILLIS * timeToLive);
    }

    /**
     * Getter accessor for attribute 'insertedDate'.
     * 
     * @return current value of 'insertedDate'
     */
    public long getInsertedDate() {
        return insertedDate;
    }

    /**
     * Getter accessor for attribute 'entry'.
     * 
     * @return current value of 'entry'
     */
    public T getEntry() {
        return entry;
    }
}
