package org.ff4j.cache;

/*
 * #%L
 * FeatureCacheProviderInMemory.java (ff4j-core) by Cedrick LUNVEN
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
import java.util.WeakHashMap;

import org.ff4j.core.Feature;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Proposition of inmemory cache implementation.
 * 
 * Warn : DO NOT USE THIS CACHE WHEN WORKING WITH EXTERNAL FEATURESTORE (as Database) and cluster application : EACH NODE GOT ITS
 * MEMORY AND AN MODIFICATION IN STORE WON'T REFRESH THIS CACHE. Please use REDIS/MEMCACHED implementations.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureCacheProviderInMemory implements FeatureCacheManager {

    /** Default TTL is one hour. */
    public static final long DEFAULT_TTL = 3600L;

    /** externalized as constant. */
    public static final long TO_MILLIS = 1000L;

    /** cache name if several caches within memory. */
    public static final String DEFAULT_CACHENAME = "ff4j-cache";

    /** Logger for the class. */
    static final Logger LOG = LoggerFactory.getLogger(FeatureCacheProviderInMemory.class);

    /** Cached Feature Map */
    private final Map<String, InMemoryCacheEntry<Feature>> cache = new WeakHashMap<String, InMemoryCacheEntry<Feature>>();

    /**
     * Time to live : The maximum number of seconds an element can exist in the cache regardless of use. The element expires at
     * this limit and will no longer be returned from the cache. The default value is 0, which means no TTL eviction takes place
     * (infinite lifetime).
     */
    private long ttl = DEFAULT_TTL;

    /** {@inheritDoc} */
    @Override
    public void clear() {
        cache.clear();
        LOG.debug("InMemoryCache has been cleared");
    }

    /** {@inheritDoc} */
    @Override
    public void evict(String featureId) {
        if (cache.containsKey(featureId)) {
            cache.remove(featureId);
            LOG.debug("evict feature {} from cache", featureId);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void put(Feature feat) {
        if (feat == null) {
            throw new IllegalArgumentException("ff4j-core: Cannot insert null feature into cache");
        }
        if (feat.getUid() == null || feat.getUid().isEmpty()) {
            throw new IllegalArgumentException("ff4j-core: Cannot insert feature with null identifier into cache");
        }
        cache.put(feat.getUid(), new InMemoryCacheEntry<Feature>(feat));
        LOG.debug("Adding {} to cache", feat.getUid());
    }

    /** {@inheritDoc} */
    @Override
    public Feature get(String featureId) {
        InMemoryCacheEntry<Feature> ice = cache.get(featureId);
        if (ice != null) {
            // a feature is stored in cache with this identifier
            if ((System.currentTimeMillis() - ice.getInsertedDate()) >= (TO_MILLIS * ttl)) {
                // it has reach its time-to-live
                LOG.debug("{} has reached its maximum ttl, evict from cache", featureId);
                evict(featureId);
            } else {
                // return cached value
                return ice.getEntry();
            }
        }
        // not in cache
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Object getNativeCache() {
        return cache;
    }

    /**
     * Getter accessor for attribute 'ttl'.
     * 
     * @return current value of 'ttl'
     */
    public long getTtl() {
        return ttl;
    }

    /**
     * Setter accessor for attribute 'ttl'.
     * 
     * @param ttl
     *            new value for 'ttl '
     */
    public void setTtl(long ttl) {
        this.ttl = ttl;
    }

}