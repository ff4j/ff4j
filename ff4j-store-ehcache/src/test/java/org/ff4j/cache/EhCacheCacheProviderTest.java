package org.ff4j.cache;

/*-
 * #%L
 * ff4j-store-ehcache
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import org.ff4j.core.FeatureStore;
import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.junit.After;
import org.junit.Assert;
import org.junit.Test;

import net.sf.ehcache.Cache;
import net.sf.ehcache.config.CacheConfiguration;
import net.sf.ehcache.config.Configuration;

/**
 * Class to test the EHCache {@link FeatureCacheProviderEhCache}.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class EhCacheCacheProviderTest extends FeatureStoreTestSupport {

    /** Cache Manager. */
    private final FF4JCacheManager cache = new FeatureCacheProviderEhCache();
    
    private FeatureStore  store  = new InMemoryFeatureStore("test-ehcacheProvider.xml");
    
    private PropertyStore pstore = new InMemoryPropertyStore("test-ehcacheProvider.xml");

    /** {@inheritDoc} */
    @Override
    public FeatureStore initStore() {
       
        return new FF4jCacheProxy(store, pstore,  cache);
    }

    /**
     * Clear all elements
     */
    @After
    public void tearDown() {
        ((Cache) cache.getFeatureNativeCache()).removeAll();
    }

    @Test
    public void initCacheProvider() {
        Configuration managerConfiguration = new Configuration();
        managerConfiguration.name("config");
        managerConfiguration.setDefaultCacheConfiguration(new CacheConfiguration("toto", 1000));
        
        FeatureCacheProviderEhCache fcec = new FeatureCacheProviderEhCache(managerConfiguration);
        Assert.assertNotNull(fcec.getCacheProviderName());
        fcec.setCacheFeatures(fcec.getCacheFeatures());
        fcec.setCacheProperties(fcec.getCacheProperties());
        fcec.setCacheConfiguration(fcec.getCacheConfiguration());
        fcec.setCacheManager(fcec.getCacheManager());
    }

}
