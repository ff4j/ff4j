package org.ff4j.store;

/*
 * #%L
 * ff4j-store-jcache
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


import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.ff4j.cache.FF4jJCacheManager;
import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.utils.Util;

/**
 * Generic {@link PropertyStore} to persist properties in a JCache (JSR107) compliant storage.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class PropertyStoreJCache extends AbstractPropertyStore {

    /** Cache Manager. */ 
    private FF4jJCacheManager cacheManager;
    
    /**
     * Default Constructor.
     */
    public PropertyStoreJCache(String cachingProviderClassName) {
        this(new FF4jJCacheManager(cachingProviderClassName));
    }
    
    /**
     * Initialization with cache manager.
     *
     * @param cacheManager
     */
    public PropertyStoreJCache(FF4jJCacheManager cacheManager) {
        this.cacheManager = cacheManager;
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean existProperty(String name) {
        Util.assertParamHasLength(name, "Property name");
        return getCacheManager().getProperty(name) != null;
    }

    /** {@inheritDoc} */
    @Override
    public <T> void createProperty(Property<T> property) {
        if (property == null) {
            throw new IllegalArgumentException("Property cannot be null nor empty");
        }
        if (existProperty(property.getName())) {
            throw new PropertyAlreadyExistException(property.getName());
        }
        getCacheManager().putProperty(property);
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> readProperty(String name) {
        if (!existProperty(name)) {
            throw new PropertyNotFoundException(name);
        }
        return getCacheManager().getProperty(name);
    }

    /** {@inheritDoc} */
    @Override
    public void updateProperty(String name, String newValue) {
        Property<?> fp = readProperty(name);
        fp.setValueFromString(newValue);
        updateProperty(fp);
    }

    /** {@inheritDoc} */
    @Override
    public <T> void updateProperty(Property<T> property) {
        if (property == null) {
            throw new IllegalArgumentException("Property cannot be null");
        }
        if (!existProperty(property.getName())) {
            throw new PropertyNotFoundException(property.getName());
        }
        getCacheManager().putProperty(property);
    }

    /** {@inheritDoc} */
    @Override
    public void deleteProperty(String name) {
        if (!existProperty(name)) {
            throw new PropertyNotFoundException(name);
        }
        getCacheManager().evictProperty(name);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Property<?>> readAllProperties() {
        Map<String, Property<?>> myMap = new HashMap<>();
        getCacheManager().getPropertiesCache().forEach(e->myMap.put(e.getKey(), e.getValue()));
        return myMap;
    }
    
    /** {@inheritDoc} */
    @Override
    public Set<String> listPropertyNames() {
        Set<String> setOfPropertyNames = new HashSet<>();
        getCacheManager().getPropertiesCache().forEach(e->setOfPropertyNames.add(e.getKey()));
        return setOfPropertyNames;
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        getCacheManager().getPropertiesCache().removeAll();
    }
    
    /**
     * Getter accessor for attribute 'cacheManager'.
     *
     * @return
     *       current value of 'cacheManager'
     */
    public FF4jJCacheManager getCacheManager() {
        return cacheManager;
    }

    /**
     * Setter accessor for attribute 'cacheManager'.
     * @param cacheManager
     *      new value for 'cacheManager '
     */
    public void setCacheManager(FF4jJCacheManager cacheManager) {
        this.cacheManager = cacheManager;
    }

}
