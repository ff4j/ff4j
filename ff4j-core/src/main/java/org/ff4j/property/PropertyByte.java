package org.ff4j.property;

import org.ff4j.exception.InvalidPropertyTypeException;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2015 FF4J
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
 * Boolean Property.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class PropertyByte extends Property< Byte > {

    /**
     * represent a boolean propertu.
     */
    private static final long serialVersionUID = -3108407128242804565L;
    
    /**
     * Default constructor.
     */
    public PropertyByte() {
    }
    
    /**
     * Constructor by property name.
     *
     * @param name
     *      property name
     */
    public PropertyByte(String name) {
        super(name);
    }
    
    /**
     * Constructor by string expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      flag value
     */
    public PropertyByte(String uid, String value) {
       super(uid, value);
    }
    
    /**
     * Constructor with name , value and target available values
     *
     * @param name
     *      current name
     * @param value
     *      current value
     */    
    public PropertyByte(String name, Byte value, Byte... fixed) {
        super(name, value, fixed);
    }
    
    /** {@inheritDoc} */
    @Override
    public Byte fromString(String v) {
        if (v == null) return null;
        if (v.length() == 0 || v.length()  > 3) {
            throw new InvalidPropertyTypeException("A byte is a single byte, a single character");
        }
        return v.getBytes()[0];
    }

}
