package org.ff4j.ignite.store;

import org.apache.ignite.Ignite;
import org.apache.ignite.Ignition;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.store.PropertyStoreJCache;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.AfterClass;
import org.junit.BeforeClass;

/**
 * Test to work with Redis as a store.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class PropertyStoreIgniteTest extends PropertyStoreTestSupport {
   
    /** ignite. */
    private static Ignite ignite;
    
    @BeforeClass
    public static void startIgnite() {
        ignite = Ignition.start();
    }
    
    /** {@inheritDoc} */
    @Override
    protected PropertyStore initPropertyStore() {
        PropertyStoreJCache propertyTestStore = new PropertyStoreIgnite(ignite);
        propertyTestStore.importPropertiesFromXmlFile("ff4j.xml");
        return propertyTestStore;
    }
    
    @AfterClass
    public static void stopIgnite() {
        if (ignite != null) {
            ignite.close();
        }
    }
    
}
