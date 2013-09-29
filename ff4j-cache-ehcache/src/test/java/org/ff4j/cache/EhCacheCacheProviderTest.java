package org.ff4j.cache;

import org.ff4j.store.FeatureStore;
import org.ff4j.store.InMemoryFeatureStore;

public class EhCacheCacheProviderTest extends AbstractStoreTest {

    /** {@inheritDoc} */
    @Override
    public FeatureStore initStore() throws Exception {
        return new FeatureStoreCacheProxy(new InMemoryFeatureStore(), new FeatureCacheProviderEhCache());
    }

}
