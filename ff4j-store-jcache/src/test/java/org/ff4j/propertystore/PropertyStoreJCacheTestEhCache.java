package org.ff4j.propertystore;

import org.ehcache.jsr107.EhcacheCachingProvider;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.store.PropertyStoreJCache;
import org.ff4j.test.propertystore.AbstractPropertyStoreJunitTest;

/**
 * Test to work with Redis as a store.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class PropertyStoreJCacheTestEhCache extends AbstractPropertyStoreJunitTest {
   
    /** {@inheritDoc} */
    @Override
    protected PropertyStore initPropertyStore() {
        PropertyStoreJCache riCacheStore = new PropertyStoreJCache(EhcacheCachingProvider.class.getName());
        riCacheStore.importPropertiesFromXmlFile("ff4j.xml");
        return riCacheStore;
    }  

}
