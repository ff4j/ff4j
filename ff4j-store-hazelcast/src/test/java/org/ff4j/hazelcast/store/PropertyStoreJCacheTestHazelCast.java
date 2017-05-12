package org.ff4j.hazelcast.store;

import org.ff4j.property.store.PropertyStore;
import org.ff4j.store.PropertyStoreJCache;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;

/**
 * Test to work with Redis as a store.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class PropertyStoreJCacheTestHazelCast extends PropertyStoreTestSupport {
   
    /** {@inheritDoc} */
    @Override
    protected PropertyStore initPropertyStore() {
        PropertyStoreJCache hazelCastStore = new PropertyStoreHazelCast();
        hazelCastStore.importPropertiesFromXmlFile("ff4j.xml");
        return hazelCastStore;
    }
    
}
