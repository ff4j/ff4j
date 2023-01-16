package org.ff4j.cache;

import java.time.Duration;
import java.time.temporal.ChronoUnit;

/**
 * Cache entry with object and inserted Date.
 *
 * @param <T>
 *     object to enter cache
 */
public final class FF4jCacheEntry<T> {

    /** TTL of this entry. */
    private Duration timeToLive = Duration.of(3600, ChronoUnit.SECONDS);
    
    /** Insertion date, allows computing time-to-live. */
    private final long insertedDate;

    /** Current entry to cache. */
    private final T entry;

    /**
     * Parameterized constructor with target cached object.
     * 
     * @param entry
     *            cached object
     */
    public FF4jCacheEntry(T entry) {
        this.entry = entry;
        this.insertedDate = System.currentTimeMillis();
    }
    
    /**
     * Parameterized constructor with target cached object.
     * 
     * @param entry
     *          cached object
     * @param timeToLive
     *          time the entry should stay in cache
     */
    public FF4jCacheEntry(T entry, Duration timeToLive) {
        this(entry);
        this.timeToLive   = timeToLive;
    }
    
    /**
     * Compute the timeout property.
     *
     * @return
     *      time to live
     */
    public boolean isOutdated() {
        return (System.currentTimeMillis() - getInsertedDate()) >= (timeToLive.toMillis());
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
