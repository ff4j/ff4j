package org.ff4j.ignite.store;

import org.apache.ignite.Ignite;
import org.apache.ignite.Ignition;
import org.ff4j.core.FeatureStore;
import org.ff4j.ignite.store.FeatureStoreIgnite;
import org.ff4j.store.FeatureStoreJCache;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.junit.AfterClass;
import org.junit.BeforeClass;

/**
 * Test to work with Redis as a store.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreIgniteTest extends FeatureStoreTestSupport {
   
    /** ignite. */
    private static Ignite ignite;
    
    @BeforeClass
    public static void startIgnite() {
        ignite = Ignition.start();
    }
    
    /** {@inheritDoc} */
    @Override
    protected FeatureStore initStore() {
        FeatureStoreJCache featureStore = new FeatureStoreIgnite(ignite);
        featureStore.importFeaturesFromXmlFile("ff4j.xml");
        return featureStore;
    }
    
    @AfterClass
    public static void stopIgnite() {
        if (ignite != null) {
            ignite.close();
        }
    }

}
