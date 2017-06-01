package org.ff4j.hazelcast.store;

import org.ff4j.core.FeatureStore;
import org.ff4j.store.FeatureStoreJCache;
import org.ff4j.test.store.FeatureStoreTestSupport;

/**
 * Test to work with Redis as a store.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreHazelCastTest extends FeatureStoreTestSupport {
   
    /** {@inheritDoc} */
    @Override
    protected FeatureStore initStore() {
        FeatureStoreJCache hazelCastStore = new FeatureStoreHazelCast();
        hazelCastStore.importFeaturesFromXmlFile("ff4j.xml");
        return hazelCastStore;
    }

}
