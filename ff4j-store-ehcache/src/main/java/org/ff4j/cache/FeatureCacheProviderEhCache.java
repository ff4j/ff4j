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

import net.sf.ehcache.Cache;
import net.sf.ehcache.CacheManager;
import net.sf.ehcache.Element;

import org.ff4j.core.Feature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Cache-aside implementation with EHCACHE.
 * 
 * Application code uses the cache directly. This means that application code which accesses the system-of-record (SOR) should
 * consult the cache first, and if the cache contains the data, then return the data directly from the cache, bypassing the SOR.
 * 
 * * Warn : DO NOT USE THIS CACHE WHEN WORKING WITH EXTERNAL FEATURESTORE (as Database) and cluster application : EACH NODE GOT
 * ITS MEMORY AND AN MODIFICATION IN STORE WON'T REFRESH THIS CACHE. Please use REDIS/MEMCACHED implementations.
 */
public class FeatureCacheProviderEhCache implements FeatureCacheManager {

    /** Logger for the class. */
    private static final Logger LOG = LoggerFactory.getLogger(FeatureStoreCacheProxy.class);

    /** Default TTL is one hour. */
    public static final long DEFAULT_TIME_TO_LIVE = 120L;

    /** Default time to idle. */
    public static final long DEFAULT_TIME_TO_IDLE = 120L;

    /** Default cache name. */
    public static final String DEFAULT_CACHENAME = "ff4j-cache";

    /** Eh Cache - cache-aside mode utlization. */
    private Cache cache = null;

    /**
     * Default constructor to allow IoC.
     */
    public FeatureCacheProviderEhCache() {}

    /** {@inheritDoc} */
    @Override
    public void clear() {
        getCache().flush();
    }

    /** {@inheritDoc} */
    @Override
    public void evict(String featureId) {
        getCache().remove(featureId);
    }

    /** {@inheritDoc} */
    @Override
    public void put(Feature feat) {
        getCache().put(new Element(feat.getUid(), feat));
    }

    /** {@inheritDoc} */
    @Override
    public Feature get(String featureId) {
        Element e = getCache().get(featureId);
        if (e != null) {
            return (Feature) e.getObjectValue();
        }
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Object getNativeCache() {
        return getCache();
    }

    /** {@inheritDoc} */
    @Override
    public String getCacheProviderName() {
        return "EHCACHE";
    }

    /**
     * Retrieve cache. If not exist, created default cache
     * 
     * @return
     */
    private Cache getCache() {
        if (cache == null) {
            initializeCache();
        }
        return cache;
    }

    /**
     * Ininitialize cache
     */
    private void initializeCache() {
        CacheManager cacheManager = CacheManager.create();
        if (!cacheManager.cacheExists(DEFAULT_CACHENAME)) {
            cacheManager.addCache(DEFAULT_CACHENAME);
        }
        cache = cacheManager.getCache(DEFAULT_CACHENAME);
        LOG.debug("CacheManager initialized as '{}'", cache.getName());
    }

}
