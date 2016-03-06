package org.ff4j.audit.graph;

/**
 * Utility class to perform some computation. The mutable implementation is more efficient because
 * there is less instanciations.
 * 
 * @see http://stackoverflow.com/questions/81346/most-efficient-way-to-increment-a-map-value-in-java
 * 
 * @author Cedrick Lunven (@clunven)
 */
public final class MutableInt {

    /** We start at 1 since we're counting. */
    int value = 1;

    /**
     * Increment value
     */
    public void inc() { 
        ++value;
    }
    
    /**
     * Read value.
     * @return
     *      current value
     */
    public int  get () { 
        return value;
    }
    
}
