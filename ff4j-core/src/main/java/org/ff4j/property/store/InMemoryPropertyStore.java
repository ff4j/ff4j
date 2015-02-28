package org.ff4j.property.store;

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

import java.util.LinkedHashMap;
import java.util.Map;

import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.AbstractProperty;
import org.ff4j.utils.Util;

/**
 * Implementation of {@link PropertyStore} to keep properties in memory.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class InMemoryPropertyStore extends AbstractPropertyStore {

    /** InMemory Feature Map */
    private Map<String, AbstractProperty<?>> properties = new LinkedHashMap<String, AbstractProperty<?>>();

    /**
     * Default Constructor 
     */
    public InMemoryPropertyStore() {
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean exist(String name) {
        Util.assertHasLength(name);
        return properties.containsKey(name);
    }
    
    /** {@inheritDoc} */
    @Override
    public <T> void create(AbstractProperty<T> value) {
        // Check Params
        Util.assertNotNull(value);
        Util.assertHasLength(value.getName());
        // Check value
        if (exist(value.getName())) {
            throw new PropertyAlreadyExistException(value.getName());
        }
        // Create
        properties.put(value.getName(), value);
    }

    /** {@inheritDoc} */
    @Override
    public AbstractProperty<?> read(String name) {
        Util.assertHasLength(name);
        if (!properties.containsKey(name)) {
            throw new PropertyNotFoundException(name);
        }
        return properties.get(name);
    }

    /** {@inheritDoc} */
    @Override
    public <T> void update(AbstractProperty<T> newValue) {
        Util.assertNotNull(newValue);
        Util.assertHasLength(newValue.getName());
        if (!exist(newValue.getName())) {
            throw new PropertyNotFoundException(newValue.getName());
        }
        // Update
        properties.put(newValue.getName(), newValue);
    }
    
    /** {@inheritDoc} */
    @Override
    public void update(String name, String newValue) {
        Util.assertHasLength(name);
        Util.assertHasLength(newValue);
        if (!exist(name)) {
            throw new PropertyNotFoundException(name);
        }
        // Update
        AbstractProperty<?> current = read(name);
        current.setValueFromString(newValue);
    } 
    
    /** {@inheritDoc} */
    @Override
    public void delete(String name) {
        Util.assertHasLength(name);
        if (!properties.containsKey(name)) {
            throw new PropertyNotFoundException(name);
        }
        // Delete
        properties.remove(name);
    }

      

}
