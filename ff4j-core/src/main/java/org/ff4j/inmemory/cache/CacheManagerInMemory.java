package org.ff4j.inmemory.cache;

import static org.ff4j.test.AssertUtils.*;

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
import java.util.Optional;
import java.util.WeakHashMap;
import java.util.stream.Stream;

import org.ff4j.FF4jEntity;
import org.ff4j.cache.CacheManager;

/**
 * Proposition of inmemory cache implementation.
 * 
 * Warn : DO NOT USE THIS CACHE WHEN WORKING WITH EXTERNAL FEATURESTORE (as Database) and cluster application : EACH NODE GOT ITS
 * MEMORY AND AN MODIFICATION IN STORE WON'T REFRESH THIS CACHE. Please use REDIS/MEMCACHED implementations.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class CacheManagerInMemory<V extends FF4jEntity<?>> implements CacheManager< String, V > {
    
    /** Cached Feature Map */
    private final Map<String, CacheEntryInMemory<V>> customCache = new WeakHashMap<String, CacheEntryInMemory<V>>();
       
    /** {@inheritDoc} */
    @Override
    public String getCacheProviderName() {
        return "In-Memory";
    }
    
    /** {@inheritDoc} */
    @Override
    public Object getNativeCache() {
        return customCache;
    }
    
    /** {@inheritDoc} */
    @Override
    public Stream<String> keys() {
        return customCache.keySet().stream();
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        customCache.clear();
    }

    /** {@inheritDoc} */
    @Override
    public void evict(String key) {
        if (customCache.containsKey(key)) {
            customCache.remove(key);
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public void put(String key, V value) {
        assertHasLength(key);
        assertNotNull(value);
        assertHasLength(value.getUid());
        customCache.put(key, new CacheEntryInMemory<V>(value));
    }
    
    /** {@inheritDoc} */
    public void put(V value) {
        put(value.getUid(), value);
    }
    
    /** {@inheritDoc} */
    public void put(V feat, long timeToLive) {
        assertNotNull(feat);
        assertHasLength(feat.getUid());
        customCache.put(feat.getUid(), new CacheEntryInMemory<V>(feat, timeToLive));
    }

    /** {@inheritDoc} */
    @Override
    public Optional < V > get(String featureId) {
        CacheEntryInMemory<V> cacheEntry = customCache.get(featureId);
        if (cacheEntry != null) {
            if (cacheEntry.hasReachTimeToLive()) {
                evict(featureId);
            } else {
                // return cached value
                return Optional.of(cacheEntry.getEntry());
            }
        }
        return Optional.empty();
    }
    
}