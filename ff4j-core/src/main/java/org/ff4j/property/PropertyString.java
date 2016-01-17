package org.ff4j.property;

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

import java.util.Set;

/**
 * Default implementation of {@link Property} as Simple string property.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class PropertyString extends Property<String> {
    
    /** serial. */
    private static final long serialVersionUID = -7894832435341748278L;

    /**
     * Default constructor.
     */
    public PropertyString() {
    }
    
    /**
     * Constructor by property name.
     *
     * @param name
     *      property name
     */
    public PropertyString(String name) {
        super(name);
    }
    
    /**
     * Constructor by property value.
     * 
     * @param name
     *      current name
     * @param value
     *      current value
     */
    public PropertyString(String name, String value) {
        super(name, value);
    }
    
    /**
     * Full Constructor.
     * 
     * @param name
     *      current name
     * @param value
     *      current value
     * @param fixed
     *      fixed values available for this property
     */
    public PropertyString(String name, String value, Set < String> fixed) {
        super(name, value, fixed.toArray(new String[0]));
    }

    /** {@inheritDoc} */
    @Override
    public String fromString(String v) {
        if (v == null) {
            throw new IllegalArgumentException("Property value cannot be null");
        }
        // Use to initialize fixedValue, cannot check them immediately
        //if (fixedValues!= null && !fixedValues.contains(v)) {
        //    throw new IllegalArgumentException("Invalid value corrects are " + fixedValues);
        // }
        return v;
    }

}
