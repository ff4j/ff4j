package org.ff4j.store;

import java.util.HashMap;
import java.util.HashSet;

/*
 * #%L
 * ff4j-store-ehcache
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


import java.util.Map;
import java.util.Set;

import org.ff4j.ehcache.FF4jEhCacheWrapper;
import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.AbstractProperty;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.utils.Util;

import net.sf.ehcache.Element;
import net.sf.ehcache.config.Configuration;

/**
 * Store {@link AbstractProperty} into EHCache.
 * 
 * @author Cedrick Lunven (@clunven)</a>
 */
public class PropertyStoreEhCache extends AbstractPropertyStore {
    
    /** Wrap EHCACHE Manager. */
    private FF4jEhCacheWrapper wrapper;

    /**
     * Default Constructor.
     */
    public PropertyStoreEhCache() {
        wrapper = new FF4jEhCacheWrapper();
    }
    
    /**
     * Default Constructor.
     */
    public PropertyStoreEhCache(Configuration cacheConfig) {
        wrapper = new FF4jEhCacheWrapper(cacheConfig);
    }
    
    /**
     * Default Constructor.
     */
    public PropertyStoreEhCache(String xmlEhCacheConfig) {
        wrapper = new FF4jEhCacheWrapper(xmlEhCacheConfig);
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean existProperty(String name) {
        Util.assertParamNotNull(name, "Property name");
        return  (wrapper.getCacheProperties().get(name) != null);
    }

    /** {@inheritDoc} */
    @Override
    public <T> void createProperty(AbstractProperty<T> property) {
        if (property == null) {
            throw new IllegalArgumentException("Property cannot be null nor empty");
        }
        if (existProperty(property.getName())) {
            throw new PropertyAlreadyExistException(property.getName());
        }
        wrapper.getCacheProperties().put(new Element(property.getName(), property));
    }

    /** {@inheritDoc} */
    @Override
    public AbstractProperty<?> readProperty(String name) {
        if (!existProperty(name)) {
            throw new PropertyNotFoundException(name);
        }
        return  (AbstractProperty<?>) wrapper.getCacheProperties().get(name).getObjectValue();
    }

    /** {@inheritDoc} */
    @Override
    public void updateProperty(String name, String newValue) {
        AbstractProperty<?> fp = readProperty(name);
        fp.setValueFromString(newValue);
        updateProperty(fp);
    }

    /** {@inheritDoc} */
    @Override
    public <T> void updateProperty(AbstractProperty<T> property) {
        if (property == null) {
            throw new IllegalArgumentException("Property cannot be null");
        }
        if (!existProperty(property.getName())) {
            throw new PropertyNotFoundException(property.getName());
        }
        wrapper.getCacheProperties().put(new Element(property.getName(), property));
    }

    /** {@inheritDoc} */
    @Override
    public void deleteProperty(String name) {
        if (!existProperty(name)) {
            throw new PropertyNotFoundException(name);
        }
        wrapper.getCacheProperties().remove(name);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, AbstractProperty<?>> readAllProperties() {
        Map<String, AbstractProperty<?>> myMap = new HashMap<String, AbstractProperty<?>>();
        if (wrapper.getCacheProperties().getKeys() != null) {
            for (Object key : wrapper.getCacheProperties().getKeys()) {
                Element element = wrapper.getCacheProperties().get(key);
                if (element != null) {
                    AbstractProperty<?> p = (AbstractProperty<?>) wrapper.getCacheProperties().get(key).getObjectValue();
                    myMap.put((String) key, p);
                }
            }
        }
        return myMap;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> listPropertyNames() {
        Set < String > propertyNames = new HashSet<String>();
        for (Object key : wrapper.getCacheProperties().getKeys()) {
            propertyNames.add((String) key);
        }
        return propertyNames;
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        wrapper.getCacheProperties().removeAll();
    }
}
