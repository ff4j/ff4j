package org.ff4j.hbase;

import org.ff4j.core.FeatureStore;
import org.ff4j.hbase.store.FeatureStoreHBase;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.junit.Ignore;

/**
 * Unit test of {@link FeatureStore} for HBASE.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
@Ignore
public class FeatureStoreHBaseTest extends FeatureStoreTestSupport {
    
    /** HBASE_HOST. */
    private static final String HBASE_HOST = "hbase";
    
    /** HBASE_PORT. */
    private static final int HBASE_PORT = 2181;
    
    /** {@inheritDoc} */
    @Override
    protected FeatureStore initStore() {
        FeatureStoreHBase fs = new FeatureStoreHBase(new HBaseConnection(HBASE_HOST, HBASE_PORT, false));
        fs.importFeaturesFromXmlFile("ff4j.xml");
        return fs;
    } 
    
}
