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

import java.util.List;

/**
 * Default implementation of {@link AbstractProperty}.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class Property extends AbstractProperty< String>{
    
    /**
     * Default constructor..
     */
    public Property() {
    }
    
    /**
     * Default constructor.
     * @param name
     *      default.
     */
    public Property(String name) {
        super(name);
    }
    
    /**
     * Reference super constructor.
     * 
     * @param name
     *      current name
     * @param value
     *      current value
     */
    public Property(String name, String value) {
        super(name, value);
    }
    
    /**
     * Reference super constructor.
     * 
     * @param name
     *      current name
     * @param value
     *      current value
     */
    public Property(String name, String value, List < String> fixed) {
        super(name, value, fixed);
    }

    /** {@inheritDoc} */
    @Override
    public String fromString(String v) {
        if (v == null || (fixedValues!= null && !fixedValues.contains(v))) {
            throw new IllegalArgumentException("Invalid value corrects are " + fixedValues);
        }
        return v;
    }

}
