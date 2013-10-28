package org.ff4j.cache;

/*
 * #%L InMemoryCacheEntry.java (ff4j-core) by Cedrick LUNVEN %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the
 * License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import java.io.Serializable;

/**
 * Cache entry with object and inserted Date.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public final class InMemoryCacheEntry<T> implements Serializable {

    /** serial. */
    private static final long serialVersionUID = -1444331517339058103L;

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
