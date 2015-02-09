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

import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.AbstractProperty;

/**
 * Implementation of {@link PropertyStore} to keep properties in memory.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class InMemoryPropertyStore implements PropertyStore {

    /** InMemory Feature Map */
    private Map<String, AbstractProperty<?>> properties = new LinkedHashMap<String, AbstractProperty<?>>();

    /** {@inheritDoc} */
    @Override
    public <T> void create(AbstractProperty<T> value) {
        properties.put(value.getName(), value);
    }

    /** {@inheritDoc} */
    @Override
    public AbstractProperty<?> read(String name) {
        if (!properties.containsKey(name)) {
            throw new PropertyNotFoundException(name);
        }
        return properties.get(name);
    }

    @Override
    public <T> void update(String name, AbstractProperty<T> newValue) {
        create(newValue);
    }

    @Override
    public void delete(String name) {
        // TODO Auto-generated method stub
        
    }

}
