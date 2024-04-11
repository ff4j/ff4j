package org.ff4j.cassandra;

/*-
 * #%L
 * ff4j-store-cassandra
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import org.ff4j.cassandra.store.FeatureStoreCassandra;
import org.ff4j.core.FeatureStore;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.junit.AfterClass;

import com.datastax.oss.driver.api.core.CqlSession;

/**
 * Test Cassandra Store.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public abstract class AsbtractFeatureStoreCassandraTest extends FeatureStoreTestSupport {
   
    protected static CqlSession cqlSession;
    
    public abstract CqlSession initCqlSession();
    
    /** {@inheritDoc} */
    @Override
    protected FeatureStore initStore() {
        if (null == cqlSession) cqlSession = initCqlSession();
        FeatureStoreCassandra store = new FeatureStoreCassandra(cqlSession);
        store.createSchema();
        store.clear();
        store.importFeaturesFromXmlFile("test-ff4j-features.xml");
        return store;
    }
    
    @AfterClass
    public static void closeConnection() {
        cqlSession.close();
        cqlSession = null;
    }
}
