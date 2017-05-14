package org.ff4j.hbase;

import org.ff4j.core.FeatureStore;
import org.ff4j.hbase.store.FeatureStoreHBase;
import org.ff4j.test.store.FeatureStoreTestSupport;

/**
 * Unit test of {@link FeatureStore} for HBASE.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class FeatureStoreHBaseTest extends FeatureStoreTestSupport {
    
    @Override
    protected FeatureStore initStore() {
       HBaseConnection conn = new HBaseConnection("192.168.80.166", 2181, false);
       return new FeatureStoreHBase(conn);
    }
    
}
