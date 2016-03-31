package org.ff4j.cache;

/*
 * #%L ff4j-cache-ehcache %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License"); you may
 * not use this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import java.util.HashSet;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.property.Property;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.ehcache.Cache;
import net.sf.ehcache.CacheManager;
import net.sf.ehcache.Element;
import net.sf.ehcache.config.Configuration;

import static org.ff4j.ehcache.FF4JEhCacheConstants.*;

/**
 * Cache-aside implementation with EHCACHE.
 * 
 * Application code uses the cache directly. This means that application code which accesses the system-of-record (SOR) should
 * consult the cache first, and if the cache contains the data, then return the data directly from the cache, bypassing the SOR.
 * 
 * * Warn : DO NOT USE THIS CACHE WHEN WORKING WITH EXTERNAL FEATURESTORE (as Database) and cluster application : EACH NODE GOT
 * ITS MEMORY AND AN MODIFICATION IN STORE WON'T REFRESH THIS CACHE. Please use REDIS/MEMCACHED implementations.
 */
public class FeatureCacheProviderEhCache implements FF4JCacheManager {

    /** Logger for the class. */
    private static final Logger LOG = LoggerFactory.getLogger(FF4jCacheProxy.class);
    
    /** Setup Cache configuration to work with Terracotta for instance. */
    private Configuration cacheConfiguration;
    
    /** The cache manager. */
    private CacheManager cacheManager;
    
    /** Eh Cache - cache-aside mode utlization. */
    private Cache cacheFeatures = null;
    
    /** Eh Cache - cache-aside mode utlization. */
    private Cache cacheProperties = null;

    /**
     * Default constructor to allow IoC.
     */
    public FeatureCacheProviderEhCache() {
    }
    
    /**
     * Default constructor to allow IoC.
     */
    public FeatureCacheProviderEhCache(Configuration config) {
        this.cacheConfiguration = config;
    }
   
    /** {@inheritDoc} */
    @Override
    public String getCacheProviderName() {
        return "EHCACHE";
    }
    
    /** {@inheritDoc} */
    @Override
    @SuppressWarnings("unchecked")
    public Set<String> listCachedFeatureNames() {
        return new HashSet<String>(getCacheFeatures().getKeys());
    }

    /**
     * Ininitialize cache
     */
    private void initializeCache() {
        if (null == getCacheConfiguration()) {
            this.cacheManager = CacheManager.create();
        } else {
            this.cacheManager = CacheManager.create(getCacheConfiguration());  
        }
        if (!cacheManager.cacheExists(CACHENAME_FEATURES)) {
            cacheManager.addCache(CACHENAME_FEATURES);
        }
        if (!cacheManager.cacheExists(CACHENAME_PROPERTIES)) {
            cacheManager.addCache(CACHENAME_PROPERTIES);
        }
        cacheFeatures   = cacheManager.getCache(CACHENAME_FEATURES);
        cacheProperties = cacheManager.getCache(CACHENAME_PROPERTIES);
        LOG.debug("CacheManager initialized as '{}'", cacheFeatures.getName());
    }

    /** {@inheritDoc} */
    @Override
    public void clearFeatures() {
        getCacheFeatures().flush();
    }

    /** {@inheritDoc} */
    @Override
    public void clearProperties() {
        getCacheProperties().flush();
        
    }

    /** {@inheritDoc} */
    @Override
    public void evictFeature(String featureId) {
        getCacheFeatures().remove(featureId);
    }

    /** {@inheritDoc} */
    @Override
    public void evictProperty(String propertyName) {
        getCacheProperties().remove(propertyName);
    }

    /** {@inheritDoc} */
    @Override
    public void putFeature(Feature feat) {
        getCacheFeatures().put(new Element(feat.getUid(), feat));
    }

    /** {@inheritDoc} */
    @Override
    public void putProperty(Property<?> property) {
        getCacheProperties().put(new Element(property.getName(), property));
    }

    /** {@inheritDoc} */
    @Override
    public Feature getFeature(String featureId) {
        Element e = getCacheFeatures().get(featureId);
        if (e != null) {
            return (Feature) e.getObjectValue();
        }
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> getProperty(String featureId) {
        Element e = getCacheProperties().get(featureId);
        if (e != null) {
            return (Property<?>) e.getObjectValue();
        }
        return null;
    }

    /** {@inheritDoc} */
    @SuppressWarnings("unchecked")
    @Override
    public Set<String> listCachedPropertyNames() {
        return new HashSet<String>(getCacheProperties().getKeys());
    }

    /** {@inheritDoc} */
    @Override
    public Object getFeatureNativeCache() {
        return getCacheFeatures();
    }

    /** {@inheritDoc} */
    @Override
    public Object getPropertyNativeCache() {
        return getCacheProperties();
    }

    /**
     * Getter accessor for attribute 'cacheFeatures'.
     *
     * @return
     *       current value of 'cacheFeatures'
     */
    public Cache getCacheFeatures() {
        if (cacheFeatures == null) initializeCache();
        return cacheFeatures;
    }

    /**
     * Setter accessor for attribute 'cacheFeatures'.
     * @param cacheFeatures
     * 		new value for 'cacheFeatures '
     */
    public void setCacheFeatures(Cache cacheFeatures) {
        this.cacheFeatures = cacheFeatures;
    }

    /**
     * Getter accessor for attribute 'cacheProperties'.
     *
     * @return
     *       current value of 'cacheProperties'
     */
    public Cache getCacheProperties() {
        if (cacheProperties == null) initializeCache();
        return cacheProperties;
    }

    /**
     * Setter accessor for attribute 'cacheProperties'.
     * @param cacheProperties
     * 		new value for 'cacheProperties '
     */
    public void setCacheProperties(Cache cacheProperties) {
        this.cacheProperties = cacheProperties;
    }

    /**
     * Getter accessor for attribute 'cacheConfiguration'.
     *
     * @return
     *       current value of 'cacheConfiguration'
     */
    public Configuration getCacheConfiguration() {
        return cacheConfiguration;
    }

    /**
     * Setter accessor for attribute 'cacheConfiguration'.
     * @param cacheConfiguration
     * 		new value for 'cacheConfiguration '
     */
    public void setCacheConfiguration(Configuration cacheConfiguration) {
        this.cacheConfiguration = cacheConfiguration;
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

    /**
     * Setter accessor for attribute 'cacheManager'.
     * @param cacheManager
     * 		new value for 'cacheManager '
     */
    public void setCacheManager(CacheManager cacheManager) {
        this.cacheManager = cacheManager;
    }   

}
