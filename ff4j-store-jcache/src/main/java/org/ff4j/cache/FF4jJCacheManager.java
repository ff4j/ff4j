package org.ff4j.cache;

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


import java.util.HashSet;
import java.util.Set;

import javax.cache.Cache;

import org.ff4j.core.Feature;
import org.ff4j.property.Property;

/**
 * Implementation of {@link FF4JCacheManager} with reference interface JCache {@link Cache}.
 * 
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FF4jJCacheManager extends FF4jJCacheProvider implements FF4JCacheManager {    
    
    /** cache name of the features. */
    protected static final String CACHENAME_FEATURES      = "ff4jFeatures";
    
    /** cache name of the properties. */
    protected static final String CACHENAME_PROPERTIES    = "ff4jProperties";
    
    /** Implementing a JCache CacheProvider. */
    protected Cache<String, Feature> featuresCache;
    
    /** Implementing a JCache CacheProvider. */
    @SuppressWarnings("rawtypes")
    protected Cache<String, Property> propertiesCache; 
    
    /**
     * Initialisation of internal caches.
     */
    public FF4jJCacheManager() {
        // Initialization of both caching provider and manager
        super();
        featuresCache   = createCacheForFeatures();
        propertiesCache = createCacheForProperties();
    }
    
    /**
     * Initialisation of internal caches.
     */
    public FF4jJCacheManager(String cachingProviderClassName) {
        // Initialization of both caching provider and manager
        super(cachingProviderClassName);
        featuresCache   = createCacheForFeatures();
        propertiesCache = createCacheForProperties();
    }
    
    
    /**
     * Default initialisation of cache.
     *
     * @return
     */
    protected Cache<String, Feature> createCacheForFeatures() {
        if (null == getCacheManager().getCache(CACHENAME_FEATURES, String.class, Feature.class)) {
            getCacheManager().createCache(CACHENAME_FEATURES, getFeatureCacheConfiguration());
        }
        return getCacheManager().getCache(CACHENAME_FEATURES, String.class, Feature.class);
    }
    
    /**
     * Default initialisation of cache.
     *
     * @return
     */
    @SuppressWarnings("rawtypes")
    protected Cache<String, Property> createCacheForProperties() {
        if (null == getCacheManager().getCache(CACHENAME_PROPERTIES, String.class, Property.class)) {
            getCacheManager().createCache(CACHENAME_PROPERTIES, getPropertyCacheConfiguration());
        }
        return getCacheManager().getCache(CACHENAME_PROPERTIES, String.class, Property.class);
    }


    /** {@inheritDoc} */
    public Set<String> listCachedFeatureNames() {
        Set<String> keys = new HashSet<>();
        // Implements iterate, more elegant as stream how ?
        getFeaturesCache().forEach(e->keys.add(e.getKey()));
        return keys;
    }
    
    /** {@inheritDoc} */
    public Object getNativeCache() {
        return featuresCache;
    }

    /** {@inheritDoc} */
    public String getCacheProviderName() {
        return "jCache:" + featuresCache.getName() + 
                ":" + featuresCache.getCacheManager().getCachingProvider().toString();
    }   

    /** {@inheritDoc} */
    @Override
    public void clearFeatures() {
        getFeaturesCache().clear();
    }

    /** {@inheritDoc} */
    @Override
    public void clearProperties() {
        getPropertiesCache().clear();
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
        if (getPropertiesCache().containsKey(propertyName)) {
            getPropertiesCache().remove(propertyName);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void putFeature(Feature feat) {
        getFeaturesCache().put(feat.getUid(), feat);
    }

    /** {@inheritDoc} */
    @Override
    public void putProperty(Property<?> feat) {
        getPropertiesCache().put(feat.getName(), feat);
    }

    /** {@inheritDoc} */
    @Override
    public Feature getFeature(String featureId) {
        return getFeaturesCache().get(featureId);
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> getProperty(String name) {
        return getPropertiesCache().get(name);
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> listCachedPropertyNames() {
        Set<String> keys = new HashSet<>();
        // Implements iterate, more elegant as stream how ?
        getPropertiesCache().forEach(e->keys.add(e.getKey()));
        return keys;
    }

    /** {@inheritDoc} */
    @Override
    public Object getFeatureNativeCache() {
        return getFeaturesCache();
    }

    /** {@inheritDoc} */
    @Override
    public Object getPropertyNativeCache() {
        return getPropertiesCache();
    }

    /**
     * Getter accessor for attribute 'featuresCache'.
     *
     * @return
     *       current value of 'featuresCache'
     */
    public Cache<String, Feature> getFeaturesCache() {
        return featuresCache;
    }

    /**
     * Setter accessor for attribute 'featuresCache'.
     * @param featuresCache
     * 		new value for 'featuresCache '
     */
    public void setFeaturesCache(Cache<String, Feature> featuresCache) {
        this.featuresCache = featuresCache;
    }

    /**
     * Getter accessor for attribute 'propertiesCache'.
     *
     * @return
     *       current value of 'propertiesCache'
     */
    @SuppressWarnings("rawtypes")
    public Cache<String, Property> getPropertiesCache() {
        return propertiesCache;
    }

}
