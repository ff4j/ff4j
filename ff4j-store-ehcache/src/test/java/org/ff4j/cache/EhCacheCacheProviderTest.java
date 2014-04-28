package org.ff4j.cache;

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

import org.ff4j.core.FeatureStore;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.test.store.AbstractStoreJUnitTest;
import org.junit.After;

/**
 * Class to test the EHCache {@link FeatureCacheProviderEhCache}.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class EhCacheCacheProviderTest extends AbstractStoreJUnitTest {

    /** Cache Manager. */
    private final FeatureCacheManager cache = new FeatureCacheProviderEhCache();

    /** {@inheritDoc} */
    @Override
    public FeatureStore initStore() {
        FeatureStore store = new InMemoryFeatureStore("test-ehcacheProvider.xml");
        return new FeatureStoreCacheProxy(store, cache);
    }

    /**
     * Clear all elements
     */
    @After
    public void tearDown() {
        ((Cache) cache.getNativeCache()).removeAll();
    }

}
