package org.ff4j.cache.store.it;

import org.ff4j.consul.ConsulConnection;
import org.ff4j.consul.store.FeatureStoreConsul;
import org.ff4j.core.FeatureStore;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.junit.Ignore;

/**
 * Test to work with Redis as a store.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Ignore
public class FeatureStoreConsulTestIT extends FeatureStoreTestSupport {
    
    /** {@inheritDoc} */
    protected FeatureStore initStore() {
        ConsulConnection connection = new ConsulConnection();
        FeatureStoreConsul consulStore = new FeatureStoreConsul(connection);
        consulStore.importFeaturesFromXmlFile("ff4j.xml");
        return consulStore;
    }

}
