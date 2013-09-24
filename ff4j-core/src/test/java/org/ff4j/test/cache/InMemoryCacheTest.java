package org.ff4j.test.cache;

import org.ff4j.cache.InMemoryFeatureStoreCacheProxy;
import org.ff4j.store.FeatureStore;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.test.store.AbstractStoreTest;

public class InMemoryCacheTest extends AbstractStoreTest {

    /** {@inheritDoc} */
    @Override
    public FeatureStore initStore() throws Exception {
        return new InMemoryFeatureStoreCacheProxy(new InMemoryFeatureStore());
    }

    public void testPutInCache() {}

}
