package org.ff4j.store;

import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.property.AbstractPropertyStoreJunitTest;

/**
 * Work with properties
 * 
 * @author Cedrick Lunven (@clunven)</a>
 */
public class PropertyStoreEhCacheTest  extends AbstractPropertyStoreJunitTest {

    /** {@inheritDoc} */
    @Override
    protected PropertyStore initPropertyStore() {
        PropertyStoreEhCache ehcachePStore = new PropertyStoreEhCache();
        ehcachePStore.importPropertiesFromXmlFile("ff4j.xml");
        return ehcachePStore;
    }

}
