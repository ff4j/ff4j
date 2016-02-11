package org.ff4j.web.store;

import org.ff4j.FF4j;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.AfterClass;
import org.junit.BeforeClass;

import com.sun.jersey.test.framework.JerseyTest;

/**
 * Unitary test for {@link FeatureStoreHttp} on Grizzly server.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class PropertyStoreHttpTest extends PropertyStoreTestSupport {

    /** Jersy Test */
    private static JerseyTest jt = null;
    
    /**
     * Start Server Grizzly before tests on remote FeatureStore.
     */
    @BeforeClass
    public static void initializingInMemory() throws Exception {
        // Reinit FF4J
        FeatureStoreHttpTestIT.ff4j = new FF4j("test-ff4j-features.xml");
        jt = new PropertyStoreHttpTestIT();
        jt.setUp();
    }
    
    /** {@inheritDoc} */
    protected PropertyStore initPropertyStore() {
        PropertyStoreHttp fst = new PropertyStoreHttp("http://localhost:9998/ff4j");
        return fst;
    }

    /**
     * Start Server Grizzly before tests on FeatureStore
     * 
     * @throws Exception
     */
    @AfterClass
    public static void shutDownInMemeoryServer() throws Exception {
        jt.tearDown();
    }

   

}
