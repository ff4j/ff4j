package org.ff4j.store;

import org.ff4j.property.store.PropertyStore;
import org.junit.Ignore;

/**
 * Work with properties
 * 
 * @author Cedrick Lunven (@clunven)</a>
 */
@Ignore
public class PropertyStoreEhCacheTest  extends AbstractPropertyStoreJunitTest {

    /** {@inheritDoc} */
    @Override
    protected PropertyStore initPropertyStore() {
        PropertyStoreEhCache ehcachePStore = new PropertyStoreEhCache();
        ehcachePStore.importPropertiesFromXmlFile("ff4j.xml");
        return ehcachePStore;
    }

}
