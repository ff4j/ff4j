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

import org.ff4j.core.FeatureStore;
import org.ff4j.couchbase.CouchbaseConnection;
import org.ff4j.couchbase.store.FeatureStoreCouchbase;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.junit.Ignore;

/**
 * CouchBase Tests.
 *
 * @author farrellyja
 * @author Cedrick LUNVEN (@clunven)
*/
@Ignore
public class FeatureStoreCouchbaseTest extends FeatureStoreTestSupport {
    
    /** Reuse connection for tests. */
    private static CouchbaseConnection conn = null;
    
    /** {@inheritDoc} */
    @Override
    protected FeatureStore initStore() {
        if (conn == null) {
            conn = new CouchbaseConnection().addNode("127.0.0.1")
                    .userName("Administrator")
                    .password("password")
                    .featureBucketName("ff4jFeatures");
        }
        FeatureStoreCouchbase store = new FeatureStoreCouchbase(conn);
        store.clear();
        store.importFeaturesFromXmlFile("ff4j.xml");
        return store;
    }
    

}
