package org.ff4j.cache;

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
    private long insertedDate;

    /** Current entry to cache. */
    private T entry;

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
     * Setter accessor for attribute 'insertedDate'.
     * 
     * @param insertedDate
     *            new value for 'insertedDate '
     */
    public void setInsertedDate(long insertedDate) {
        this.insertedDate = insertedDate;
    }

    /**
     * Getter accessor for attribute 'entry'.
     * 
     * @return current value of 'entry'
     */
    public T getEntry() {
        return entry;
    }

    /**
     * Setter accessor for attribute 'entry'.
     * 
     * @param entry
     *            new value for 'entry '
     */
    public void setEntry(T entry) {
        this.entry = entry;
    }

}
