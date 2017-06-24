package org.ff4j.cassandra;

/*
 * #%L
 * ff4j-store-cassandra
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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

import org.cassandraunit.utils.EmbeddedCassandraServerHelper;
import org.ff4j.cassandra.store.PropertyStoreCassandra;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

/**
 * Test for property store.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
//Cassandra embedded KO
@Ignore
public class PropertyStoreCassandraTest extends PropertyStoreTestSupport {

    /** Reuse the embedded server. */
    protected static CassandraConnection conn;
    
    @BeforeClass
    public static void startEmbeddedCassandra() throws Exception {
        EmbeddedCassandraServerHelper.startEmbeddedCassandra(15000);
        conn = new CassandraConnection("127.0.0.1", 9142);
        conn.createKeySpace();
    }

    @Override
    protected PropertyStore initPropertyStore() {
        PropertyStoreCassandra cassandraStore = new PropertyStoreCassandra(conn);
        cassandraStore.createSchema();
        cassandraStore.clear();
        cassandraStore.importPropertiesFromXmlFile("test-ff4j-features.xml");
        return cassandraStore;
    }
    
    @Test
    public void testCustoms() {
        // Dummy
        CassandraConnection cc = new CassandraConnection();
        PropertyStoreCassandra cassandraStore = new PropertyStoreCassandra();
        cassandraStore.setConn(cc);
        Assert.assertNotNull(cassandraStore.getConn());
    }
}
