package org.ff4j.cache;

import org.ff4j.core.FeatureStore;
import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.After;
import org.junit.Assert;
import org.junit.Test;

/*
 * #%L
 * ff4j-cache-ehcache
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

import net.sf.ehcache.Cache;

/**
 * Class to test the EHCache {@link FeatureCacheProviderEhCache}.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class EhCacheCacheProviderBisTest extends PropertyStoreTestSupport {

    /** Cache Manager. */
    private final FF4JCacheManager cache = new FeatureCacheProviderEhCache();
    
    private FeatureStore  store  = new InMemoryFeatureStore("test-ehcacheProvider.xml");
    
    private PropertyStore pstore = new InMemoryPropertyStore("test-ehcacheProvider.xml");

    @Override
    protected PropertyStore initPropertyStore() {
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
    public void listCache() {
        Assert.assertNotNull(cache.listCachedPropertyNames());
        Assert.assertNotNull(cache.getPropertyNativeCache());
    }
        

}
