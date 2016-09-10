package org.ff4j.test.property.rd;

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

import org.ff4j.property.Property;

public class SmartProperty<T> extends Property<T> {

    /** Serial. */
    private static final long serialVersionUID = 1L;
    
    protected PropertyStrategy<T> strategy;
    
    /** {@inheritDoc} */
    @Override
    public T getValue() {
        if (strategy == null) return super.getValue();
        return strategy.getValue(getName(), null, null);
    }
    
    /** {@inheritDoc} */
    @Override
    public T fromString(String v) {
        return null;
    }
    
    
    

}
