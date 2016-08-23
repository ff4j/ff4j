package org.ff4j.cassandra.store;

/*
 * #%L
 * ff4j-store-cassandra
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


import java.util.Map;
import java.util.Set;

import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.property.store.PropertyStore;

/**
 * Implements of {@link PropertyStore} for sotre Cassandra.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class PropertyStoreCassandra extends AbstractPropertyStore {

    /** {@inheritDoc} */
    @Override
    public boolean existProperty(String name) {
        return false;
    }

    /** {@inheritDoc} */
    @Override
    public <T> void createProperty(Property<T> value) {
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> readProperty(String name) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void deleteProperty(String name) {
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Property<?>> readAllProperties() {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> listPropertyNames() {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
    }

}
