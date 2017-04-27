package org.ff4j.cache;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 Ff4J
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
import java.util.WeakHashMap;

import org.ff4j.core.Feature;
import org.ff4j.property.Property;

/**
 * Proposition of inmemory cache implementation.
 * 
 * Warn : DO NOT USE THIS CACHE WHEN WORKING WITH EXTERNAL FEATURESTORE (as Database) and cluster application : EACH NODE GOT ITS
 * MEMORY AND AN MODIFICATION IN STORE WON'T REFRESH THIS CACHE. Please use REDIS/MEMCACHED implementations.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class InMemoryCacheManager implements FF4JCacheManager {    

    /** cache name if several caches within memory. */
    public static final String DEFAULT_CACHENAME = "ff4j-cache";

    /** Cached Feature Map */
    private final Map<String, InMemoryCacheEntry<Feature>> featuresCache = 
            new WeakHashMap<String, InMemoryCacheEntry<Feature>>();
    
    /** Cached Property Map */
    private final Map<String, InMemoryCacheEntry<Property<?>>> propertyCache = 
            new WeakHashMap<String, InMemoryCacheEntry<Property<?>>>();
       
    /** {@inheritDoc} */
    @Override
    public String getCacheProviderName() {
        return "InMemory";
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> listCachedFeatureNames() {
        return featuresCache.keySet();
    }

    /** {@inheritDoc} */
    @Override
    public void clearFeatures() {
        getFeaturesCache().clear();
    }

    /** {@inheritDoc} */
    @Override
    public void clearProperties() {
        getPropertyCache().clear();
    }

    /** {@inheritDoc} */
    @Override
    public void evictFeature(String featureId) {
        if (getFeaturesCache().containsKey(featureId)) {
            getFeaturesCache().remove(featureId);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void evictProperty(String propertyName) {
        if (getPropertyCache().containsKey(propertyName)) {
            getPropertyCache().remove(propertyName);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void putFeature(Feature feat) {
        if (feat == null) {
            throw new IllegalArgumentException("ff4j-core: Cannot insert null feature into cache");
        }
        if (feat.getUid() == null || feat.getUid().isEmpty()) {
            throw new IllegalArgumentException("ff4j-core: Cannot insert feature with null identifier into cache");
        }
        getFeaturesCache().put(feat.getUid(), new InMemoryCacheEntry<Feature>(feat));
    }
    
    /** {@inheritDoc} */
    public void putFeature(Feature feat, long timeToLive) {
        if (feat == null) {
            throw new IllegalArgumentException("ff4j-core: Cannot insert null feature into cache");
        }
        if (feat.getUid() == null || feat.getUid().isEmpty()) {
            throw new IllegalArgumentException("ff4j-core: Cannot insert feature with null identifier into cache");
        }
        getFeaturesCache().put(feat.getUid(), new InMemoryCacheEntry<Feature>(feat, timeToLive));
    }

    /** {@inheritDoc} */
    @Override
    public void putProperty(Property<?> prop) {
        if (prop == null) {
            throw new IllegalArgumentException("ff4j-core: Cannot insert null property into cache");
        }
        if (prop.getName() == null || prop.getName().isEmpty()) {
            throw new IllegalArgumentException("ff4j-core: Cannot insert property with null identifier into cache");
        }
        getPropertyCache().put(prop.getName(), new InMemoryCacheEntry<Property<?>>(prop));
    }
    
    /** {@inheritDoc} */
    public void putProperty(Property<?> prop, long timeToLive) {
        if (prop == null) {
            throw new IllegalArgumentException("ff4j-core: Cannot insert null property into cache");
        }
        if (prop.getName() == null || prop.getName().isEmpty()) {
            throw new IllegalArgumentException("ff4j-core: Cannot insert property with null identifier into cache");
        }
        getPropertyCache().put(prop.getName(), new InMemoryCacheEntry<Property<?>>(prop, timeToLive));
    }

    /** {@inheritDoc} */
    @Override
    public Feature getFeature(String featureId) {
        InMemoryCacheEntry<Feature> cacheEntry = getFeaturesCache().get(featureId);
        if (cacheEntry != null) {
            if (cacheEntry.hasReachTimeToLive()) {
                evictFeature(featureId);
            } else {
                // return cached value
                return cacheEntry.getEntry();
            }
        }
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> getProperty(String pName) {
        InMemoryCacheEntry<Property<?>> cacheEntry = getPropertyCache().get(pName);
        if (cacheEntry != null) {
            if (cacheEntry.hasReachTimeToLive()) {
                evictProperty(pName);
            } else {
                // return cached value
                return cacheEntry.getEntry();
            }
        }
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> listCachedPropertyNames() {
        return propertyCache.keySet();
    }

    /** {@inheritDoc} */
    @Override
    public Object getFeatureNativeCache() {
        return getFeaturesCache();
    }

    /** {@inheritDoc} */
    @Override
    public Object getPropertyNativeCache() {
        return getPropertyCache();
    }

    /**
     * Getter accessor for attribute 'featuresCache'.
     *
     * @return
     *       current value of 'featuresCache'
     */
    public Map<String, InMemoryCacheEntry<Feature>> getFeaturesCache() {
        return featuresCache;
    }

    /**
     * Getter accessor for attribute 'propertyCache'.
     *
     * @return
     *       current value of 'propertyCache'
     */
    public Map<String, InMemoryCacheEntry<Property<?>>> getPropertyCache() {
        return propertyCache;
    }   

}