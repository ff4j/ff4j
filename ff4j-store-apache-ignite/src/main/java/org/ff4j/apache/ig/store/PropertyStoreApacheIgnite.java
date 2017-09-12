package org.ff4j.apache.ig.store;

/*
 * #%L
 * ff4j-store-apache-ignite
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;
import javax.cache.Cache;
import org.apache.ignite.IgniteCache;
import org.ff4j.core.FeatureStore;
import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.utils.Util;

/**
 * Implementation of {@link FeatureStore} for apache ignite.
 *
 */
public class PropertyStoreApacheIgnite extends AbstractPropertyStore {

    private IgniteCache<String, Property> igniteCache;

    public PropertyStoreApacheIgnite(IgniteCache<String, Property> igniteCache) {
        this.igniteCache = igniteCache;
    }

    @Override
    public boolean existProperty(String name) {
        Util.assertParamHasLength(name, "Property name");
        return igniteCache.containsKey(name);
    }

    @Override
    public <T> void createProperty(Property<T> property) {
        if (property == null) {
            throw new IllegalArgumentException("Property cannot be null nor empty");
        }
        if (existProperty(property.getName())) {
            throw new PropertyAlreadyExistException(property.getName());
        }
        igniteCache.put(property.getName(), property);
    }

    @Override
    public Property<?> readProperty(String name) {
        if (!existProperty(name)) {
            throw new PropertyNotFoundException(name);
        }
        return igniteCache.get(name);
    }

    @Override
    public void deleteProperty(String name) {
        if (!existProperty(name)) {
            throw new PropertyNotFoundException(name);
        }
        igniteCache.remove(name);
    }

    @Override
    public Map<String, Property<?>> readAllProperties() {
        return StreamSupport.stream(igniteCache.spliterator(), false)
                .collect(Collectors.toMap(Cache.Entry::getKey, Cache.Entry::getValue));
    }

    @Override
    public Set<String> listPropertyNames() {
        return StreamSupport.stream(igniteCache.spliterator(), false)
                .map(Cache.Entry::getKey)
                .collect(Collectors.toSet());
    }

    @Override
    public void clear() {
        igniteCache.clear();
    }
}
