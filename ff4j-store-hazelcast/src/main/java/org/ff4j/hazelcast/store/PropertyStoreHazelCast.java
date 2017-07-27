package org.ff4j.hazelcast.store;

/*
 * #%L
 * ff4j-store-hazelcast
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import java.util.Properties;

import org.ff4j.cache.FF4JCacheManager;
import org.ff4j.core.FeatureStore;
import org.ff4j.hazelcast.CacheManagerHazelCast;
import org.ff4j.store.FeatureStoreJCache;
import org.ff4j.store.PropertyStoreJCache;

import com.hazelcast.config.Config;

/**
 * Implementation of {@link FeatureStore} for hazelcast.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyStoreHazelCast extends PropertyStoreJCache {

    /**
     * Default constructor.
     */
    public PropertyStoreHazelCast() {
        this(new CacheManagerHazelCast());
    }
    
    /**
     * Default constructor.
     */
    public PropertyStoreHazelCast(String xmlConfigFileName, Properties systemProperties) {
        this(new CacheManagerHazelCast(xmlConfigFileName, systemProperties));
    }
            
    /**
     * Leverage on JCACHE but initialize from Hazelcast.
     *
     * @param cacheManager
     */
    public PropertyStoreHazelCast(Config hazelcastConfig, Properties systemProperties) {
        this(new CacheManagerHazelCast(hazelcastConfig, systemProperties));
    }
    
    /**
     * Init from hazelcast, cast manager (logic in {@link FeatureStoreJCache}).
     * 
     * @param cacheManager
     *      implementation of {@link FF4JCacheManager} for hazel cast
     */
    public PropertyStoreHazelCast(CacheManagerHazelCast cacheManager) {
        super(cacheManager);
    }

}
