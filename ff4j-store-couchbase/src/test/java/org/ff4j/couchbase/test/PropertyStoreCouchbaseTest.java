package org.ff4j.couchbase.test;

/*
 * #%L
 * ff4j-store-couchbase
 * %%
 * Copyright (C) 2013 - 2017 FF4J
 * %%
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */

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
