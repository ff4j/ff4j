package org.ff4j.couchbase.test;

import org.ff4j.core.FeatureStore;
import org.ff4j.couchbase.CouchbaseConnection;
import org.ff4j.couchbase.store.FeatureStoreCouchbase;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.junit.Ignore;

/**
 * CouchBase Tests.
 *
 * @author farrellyja
 * @author Cedrick LUNVEN (@clunven)
*/
@Ignore
public class FeatureStoreCouchbaseTest extends FeatureStoreTestSupport {
    
    /** Reuse connection for tests. */
    private static CouchbaseConnection conn = null;
    
    /** {@inheritDoc} */
    @Override
    protected FeatureStore initStore() {
        if (conn == null) {
            conn = new CouchbaseConnection().addNode("127.0.0.1")
                    .userName("Administrator")
                    .password("password")
                    .featureBucketName("ff4jFeatures");
        }
        FeatureStoreCouchbase store = new FeatureStoreCouchbase(conn);
        store.clear();
        store.importFeaturesFromXmlFile("ff4j.xml");
        return store;
    }
    

}
