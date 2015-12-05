package org.ff4j.store;

import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.propertystore.AbstractPropertyStoreJunitTest;

/**
 * Work with properties
 * 
 * @author Cedrick Lunven (@clunven)</a>
 */

public class PropertyStoreEhCacheTest extends AbstractPropertyStoreJunitTest {

    /** {@inheritDoc} */
    @Override
    protected PropertyStore initPropertyStore() {
        PropertyStoreEhCache ehcachePStore = new PropertyStoreEhCache();
        ehcachePStore.importPropertiesFromXmlFile("ff4j-properties.xml");
        return ehcachePStore;
    }

}
