package org.ff4j.propertystore;

import org.ff4j.property.store.PropertyStore;
import org.ff4j.store.PropertyStoreJCache;
import org.ff4j.test.propertystore.AbstractPropertyStoreJunitTest;
import org.jsr107.ri.spi.RICachingProvider;

/**
 * Test to work with Redis as a store.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class PropertyStoreJCacheTestRICache extends AbstractPropertyStoreJunitTest {
   
    /** {@inheritDoc} */
    @Override
    protected PropertyStore initPropertyStore() {
        PropertyStoreJCache riCacheStore = new PropertyStoreJCache(RICachingProvider.class.getName());
        riCacheStore.importPropertiesFromXmlFile("ff4j.xml");
        return riCacheStore;
    }  

}
