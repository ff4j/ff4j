package org.ff4j.cache.ignite;

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


import javax.cache.Cache;
import javax.cache.CacheManager;
import javax.cache.spi.CachingProvider;

import org.apache.ignite.Ignite;
import org.apache.ignite.Ignition;
import org.apache.ignite.cache.CacheAtomicityMode;
import org.apache.ignite.configuration.CacheConfiguration;
import org.ff4j.cache.FF4jJCacheManager;
import org.ff4j.core.Feature;
import org.ff4j.property.Property;

/**
 * Proposition of specialization of {@link FF4jJCacheManager} for Apache Ignite.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class FeatureCacheProviderIgnite extends FF4jJCacheManager {
    
    /** Internal HazelCast Settings. */
    private Ignite ignite = null;
    
    /** {@inheritDoc} */
    @Override
    public CachingProvider initCachingProvider(String className) {
        ignite = Ignition.ignite();
        //Ignition.start("examples/config/example-cache.xml")
        return null;
    }
    
    /** {@inheritDoc} */
    @Override
    protected CacheManager initCacheManager() {
        return null;
    }
    
    /** {@inheritDoc} */
    @Override
    protected Cache<String, Feature> createCacheForFeatures() {
        CacheConfiguration<String, Feature> cfg = new CacheConfiguration<String, Feature>();
        cfg.setName(CACHENAME_FEATURES);
        cfg.setAtomicityMode(CacheAtomicityMode.TRANSACTIONAL);
        return ignite.getOrCreateCache(cfg);
    }
    
    /** {@inheritDoc} */
    @Override
    @SuppressWarnings("rawtypes")
    protected Cache<String, Property> createCacheForProperties() {
        CacheConfiguration<String, Property> cfg = new CacheConfiguration<String, Property>();
        cfg.setName(CACHENAME_PROPERTIES);
        cfg.setAtomicityMode(CacheAtomicityMode.TRANSACTIONAL);
        return ignite.getOrCreateCache(cfg);
    }

}
