package org.ff4j.audit;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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

/**
 * Utility class to perform some computation. The mutable implementation is more efficient because
 * there is less instanciations.
 * 
 * @see http://stackoverflow.com/questions/81346/most-efficient-way-to-increment-a-map-value-in-java
 * 
 * @author Cedrick Lunven (@clunven)
 */
public final class MutableHitCount {

    int value = 0;
    
    /**
     * Default constructor
     */
    public MutableHitCount() {
    }
    
    /**
     * Paramtered constructor.
     *
     * @param startValue
     *      start value to count
     */
    public MutableHitCount(int startValue) {
       this.value = startValue;
    }

    /**
     * Increment value
     */
    public void inc() { 
        ++value;
    }
    
    public void incBy(int operand) {
        value += operand;
    }
    
    /**
     * Read value.
     * @return
     *      current value
     */
    public int  get () { 
        return value;
    }
    
    /** {@inheritDoc} */
    public String toString() {
        return String.valueOf(get());
    }

    
}
