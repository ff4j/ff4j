package org.ff4j.terracotta.it;

/*
 * #%L ff4j-store-redis %% Copyright (C) 2013 - 2014 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License"); you
 * may not use this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import java.util.Map;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.ehcache.FF4jEhCacheWrapper;
import org.ff4j.store.FeatureStoreEhCache;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.junit.After;
import org.junit.Ignore;

import net.sf.ehcache.config.CacheConfiguration;
import net.sf.ehcache.config.Configuration;
import net.sf.ehcache.config.MemoryUnit;
import net.sf.ehcache.config.TerracottaClientConfiguration;
import net.sf.ehcache.config.TerracottaConfiguration;

/**
 * Test to work with Redis as a store.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 * 
 * Working but quite slow to test all on each build
 */
@Ignore
public class FeatureStoreTerracottaTestIT extends FeatureStoreTestSupport {

    /** Terracotta URL. */
    private static final String TERRACOTTA_URL = "localhost:9510";
    
    /** {@inheritDoc} */
    @Override
    protected FeatureStore initStore() {
        
        // Configuration to wirk with Terracotta
        Configuration managerConfiguration = new Configuration();
        managerConfiguration.name("config")
            
            .terracotta(new TerracottaClientConfiguration().url(TERRACOTTA_URL))
            
            .defaultCache(new CacheConfiguration()
                .maxBytesLocalHeap(128, MemoryUnit.MEGABYTES)
                .terracotta(new TerracottaConfiguration()))
            
            .cache(new CacheConfiguration()
                .name(FF4jEhCacheWrapper.CACHENAME_FEATURES)
                .maxBytesLocalHeap(128, MemoryUnit.MEGABYTES)
                .terracotta(new TerracottaConfiguration())
            );

        FeatureStoreEhCache ehcacheStore = new FeatureStoreEhCache(managerConfiguration);
        ehcacheStore.importFeaturesFromXmlFile("ff4j.xml");
        return ehcacheStore;
    }

    /**
     * Clean store after each test (avoid duplication)
     */
    @After
    public void cleanStore() {
        Map<String, Feature> f = testedStore.readAll();
        for (String key : f.keySet()) {
            testedStore.delete(key);
        }
    }

}
