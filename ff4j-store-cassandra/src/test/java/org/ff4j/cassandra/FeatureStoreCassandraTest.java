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
import org.ff4j.cassandra.store.FeatureStoreCassandra;
import org.ff4j.core.FeatureStore;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;



/**
 * Unit test of {@link FeatureStore} for Cassandra.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
// Cassandra embedded KO
@Ignore
public class FeatureStoreCassandraTest extends FeatureStoreTestSupport {
   
    /** Reuse the embedded server. */
    protected static CassandraConnection conn;
    
    @BeforeClass
    public static void startEmbeddedCassandra() throws Exception {
        // Use a real server
        //conn = new CassandraConnection();
        // <--
        
        // Use Cassandra-Unit 
        EmbeddedCassandraServerHelper.startEmbeddedCassandra(15000);
        conn = new CassandraConnection("127.0.0.1", 9142);
        // <--
        conn.createKeySpace();
    }
    
    @Override
    protected FeatureStore initStore() {
       FeatureStoreCassandra cassandraStore = new FeatureStoreCassandra(conn);
       cassandraStore.createSchema();
       cassandraStore.clear();
       cassandraStore.importFeaturesFromXmlFile("ff4j.xml");
       return cassandraStore;
    }
    
    @Test
    public void testCustoms() {
        // Dummy
        CassandraConnection cc = new CassandraConnection();
        FeatureStoreCassandra cassandraStore = new FeatureStoreCassandra();
        cassandraStore.setConn(cc);
        Assert.assertNotNull(cassandraStore.getConn());
    }
    
}
