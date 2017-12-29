package org.ff4j.property.domain;

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


import org.ff4j.exception.InvalidPropertyTypeException;
import org.ff4j.property.Property;

/**
 * Boolean Property.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class PropertyBoolean extends Property< Boolean > {

    /**
     * represent a boolean propertu.
     */
    private static final long serialVersionUID = -3108407128242804565L;
    
    /**
     * Constructor by property name.
     *
     * @param name
     *      property name
     */
    public PropertyBoolean(String name) {
        this(name, false);
    }
    
    /**
     * Constructor by enum expression.
     *
     * @param uid
     *      unique name
     * @param lvlv
     *     flag value
     */
    public PropertyBoolean(String uid, boolean lvl) {
        this(uid, String.valueOf(lvl));
    }
    
    /**
     * Constructor by string expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      flag value
     */
    public PropertyBoolean(String uid, String value) {
       super(uid, value);
       setFixedValues(Boolean.TRUE, Boolean.FALSE);
    }
    
    
    
    /** {@inheritDoc} */
    @Override
    public Boolean fromString(String v) {
        if (!Boolean.TRUE.toString().equals(v.toLowerCase()) &&
            !Boolean.FALSE.toString().equals(v) ) {
            throw new InvalidPropertyTypeException("Cannot cast " + v + "to expected " + Boolean.class);
        }
        return new Boolean(v);
    }

}
