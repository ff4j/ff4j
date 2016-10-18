package org.ff4j.property;

import java.util.Set;

import org.ff4j.exception.InvalidPropertyTypeException;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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
 * Implementation of a property of type INT.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class PropertyInt extends Property< Integer > {

    /** serial. */
    private static final long serialVersionUID = -134543098672660987L;

    /**
     * Constructor by string expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      current log level
     */
    public PropertyInt() {
    }
    
    /**
     * Constructor by string expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      current log level
     */
    public PropertyInt(String uid) {
       super(uid);
    }
    
    /**
     * Constructor by string expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      current log level
     */
    public PropertyInt(String uid, String value) {
       super(uid, value);
    }
    
    /**
     * Constructor by string expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      current log level
     */
    public PropertyInt(String uid, Integer value) {
       super(uid, value);
    }

    /**
     * Constructor by string expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      current log level
     */
    public PropertyInt(String uid, Integer value, Set <Integer> fixValues) {
       super(uid, value, fixValues);
    }
    
    /** {@inheritDoc} */
    @Override
    public Integer fromString(String v) {
        try {
            return new Integer(v);
        } catch(NumberFormatException nbe) {
            throw new InvalidPropertyTypeException("Cannot cast " + v + "to expected " + Integer.class, nbe);
        }
    }
    

}
