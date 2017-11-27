package org.ff4j.couchbase.test;

import org.ff4j.couchbase.CouchbaseConnection;
import org.ff4j.couchbase.store.PropertyStoreCouchbase;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.Ignore;

/**
 * Class to TODO
 *
 * @author Cedrick LUNVEN (@clunven)
 *
 */
@Ignore
public class PropertyStoreCouchbaseTest extends PropertyStoreTestSupport {

    /** Reuse connection for tests. */
    private static CouchbaseConnection conn = null;
    
    /** {@inheritDoc} */
    @Override
    protected PropertyStore initPropertyStore() {
        if (conn == null) {
            conn = new CouchbaseConnection().addNode("127.0.0.1")
                    .userName("Administrator")
                    .password("password")
                    .propertyBucketName("ff4jProperties");
        }
        PropertyStoreCouchbase store = new PropertyStoreCouchbase(conn);
        store.clear();
        store.importPropertiesFromXmlFile("test-ff4j-features.xml");
        return store;
    }

}
