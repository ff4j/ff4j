package org.ff4j.hbase;

import org.ff4j.hbase.store.PropertyStoreHBase;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.Ignore;
import org.junit.Test;

/**
 * List of properties.
 * 
 * @author Cedrick LUNVEN (@clunven)
 */
@Ignore
public class PropertyStoreHBaseTest extends PropertyStoreTestSupport {

    /** HBASE_HOST. */
    private static final String HBASE_HOST = "hbase";
    
    /** HBASE_PORT. */
    private static final int HBASE_PORT = 2181;
    
    /** {@inheritDoc} */
    @Override
    protected PropertyStore initPropertyStore() {
        PropertyStoreHBase fs = new PropertyStoreHBase(new HBaseConnection(HBASE_HOST, HBASE_PORT, false));
        fs.importPropertiesFromXmlFile("ff4j.xml");
        return fs;
    }
    
    @Test
    public void testCreateSchema() {
        testedStore.createSchema();
    }
}
