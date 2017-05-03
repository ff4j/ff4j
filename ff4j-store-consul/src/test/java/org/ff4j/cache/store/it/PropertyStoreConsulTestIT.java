package org.ff4j.cache.store.it;

import org.ff4j.consul.ConsulConnection;
import org.ff4j.consul.store.PropertyStoreConsul;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.Ignore;

/**
 * Expect to run tests if a consul instance id UP.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
@Ignore
public class PropertyStoreConsulTestIT extends PropertyStoreTestSupport {

    /** {@inheritDoc} */
    protected PropertyStore initPropertyStore() {
        ConsulConnection  connection = new ConsulConnection();
        PropertyStoreConsul consulStore = new PropertyStoreConsul(connection);
        consulStore.importPropertiesFromXmlFile("ff4j.xml");
        return consulStore;
    }
    

}
