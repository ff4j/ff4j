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


import javax.cache.CacheManager;
import javax.cache.Caching;
import javax.cache.configuration.MutableConfiguration;
import javax.cache.spi.CachingProvider;

import org.ff4j.core.Feature;
import org.ff4j.property.Property;

public class FF4jJCacheProvider {
    
    /** The Cache Provider. */
    private CachingProvider cachingProvider;
    
    /** JCache, cache manager. */
    private CacheManager cacheManager; 
    
    /**
     * Initialization of cache.
     */
    protected FF4jJCacheProvider() {
        this(null);
    }

    /**
     * Initialization of cache.
     */
    protected FF4jJCacheProvider(String providerClassName) {
        
        // Initialisation of provider (can be null)
        cachingProvider = initCachingProvider(providerClassName);
      
        // Initialization of manager
        cacheManager = initCacheManager();
    }
    
    /**
     * Default Initialisation of {@link CachingProvider}. It will work only is there is
     * a single {@link CachingProvider} implementation within classpath. Otherwise should must
     * overriden it to initialize with your own.
     *
     * @return
     *      specialization of {@link CachingProvider} (JCache)
     */
    protected CachingProvider initCachingProvider(String cachingProviderClassname) {
        try {
            if (cachingProviderClassname == null) {
                return Caching.getCachingProvider();
            }
            return Caching.getCachingProvider(cachingProviderClassname);
        } catch(RuntimeException re) {
            /* Some cache implementation do not provide CachingProvider but the cacheManager
             * work properly. As a consequence, caching provider can be null and should not throw
             * caching exception.
             */
            return null;
        }
    }
    
    /**
     * Initialization of cache manager. Default implementation rely on the {@link CachingProvider} as
     * expected by the JSR 107 but some cache implementation do not provide caching provider.
     *
     * @return
     *      initialisation of cache manager
     */
    protected CacheManager initCacheManager() {
        if (cachingProvider == null) {
            throw new IllegalArgumentException("Cannot initialize cacheManager as CachingProvider is empty, please check 'initCachingProvider'");
        }
        return cachingProvider.getCacheManager();
    }
     
    /**
     * Initialize cache configuration, could be overriden.
     *
     * @return
     *      cache default configuration
     */
    protected MutableConfiguration< String, Feature> getFeatureCacheConfiguration() {
        MutableConfiguration<String, Feature> featuresCacheConfig = new MutableConfiguration<>();        
        featuresCacheConfig.setTypes(String.class, Feature.class);
        featuresCacheConfig.setStoreByValue(true);
        featuresCacheConfig.setStatisticsEnabled(false);
        return featuresCacheConfig;
    }
    
    
    /**
     * Initialize cache configuration, could be overriden.
     *
     * @return
     *      cache default configuration
     */
    @SuppressWarnings("rawtypes")
    protected MutableConfiguration< String, Property> getPropertyCacheConfiguration() {
        MutableConfiguration<String, Property> propertiesCacheConfig = new MutableConfiguration<>();        
        propertiesCacheConfig.setTypes(String.class, Property.class);
        propertiesCacheConfig.setStoreByValue(true);
        propertiesCacheConfig.setStatisticsEnabled(false);
        return propertiesCacheConfig;
    }
    
    /**
     * Getter accessor for attribute 'cachingProvider'.
     *
     * @return
     *       current value of 'cachingProvider'
     */
    public CachingProvider getCachingProvider() {
        return cachingProvider;
    }

    /**
     * Getter accessor for attribute 'cacheManager'.
     *
     * @return
     *       current value of 'cacheManager'
     */
    public CacheManager getCacheManager() {
        return cacheManager;
    }

}
