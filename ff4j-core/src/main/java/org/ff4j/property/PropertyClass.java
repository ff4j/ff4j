package org.ff4j.property;

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
 * Implementation of Property for java {@link Class}.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class PropertyClass extends Property< Class<?> > {

    /** Serial. */
    private static final long serialVersionUID = 1215847831344778135L;

    /**
     * Default constructor.
     */
    public PropertyClass() {
    }
    
    /**
     * Constructor by property name.
     *
     * @param name
     *      property name
     */
    public PropertyClass(String name) {
        super(name);
    }
    
    /**
     * Constructor by string expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      current double value
     */
    public PropertyClass(String uid, String value) {
       super(uid, value);
    }
    
    /**
     * Constructor by T expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      current double value
     */
    public PropertyClass(String uid, Class<?> value) {
       super(uid, value);
    }
    
    /** {@inheritDoc} */
    @Override
    public Class<?> fromString(String v) {
        try {
            return Class.forName(v);
        } catch (ClassNotFoundException e) {
            throw new IllegalArgumentException("The target class has not been found", e);
        }
    }
    
    /** 
     * Serialized value as String
     *
     * @return
     *      current value as a string or null
     */
    @Override
    public String asString() {
        if (value == null) {
            return null;
        }
        return value.getName();
    }

}
