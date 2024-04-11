package org.ff4j.arangodb.store;

/*-
 * #%L
 * ff4j-store-arangodb
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


import org.ff4j.core.FeatureStore;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.junit.ClassRule;
import org.junit.Ignore;

import com.arangodb.ArangoDB;
import com.arangodb.ArangoDatabase;

/**
 * ArangoDB feature store tests
 */
@Ignore
public class FeatureStoreArangoDBIT extends FeatureStoreTestSupport {

    @ClassRule
    public static ArangoDBTestContainer container = new ArangoDBTestContainer();

    @Override
    protected FeatureStore initStore() {
        ArangoDB arangoDB = new ArangoDB.Builder()
                .host("localhost", container.getFirstMappedPort())
                .user("root")
                .password("root")
                .acquireHostList(false).build();
        ArangoDatabase db = arangoDB.db("ff4j-test");

        FeatureStoreArangoDB store = new FeatureStoreArangoDB(db.collection("features"));
        store.createSchema();
        store.clear();
        store.importFeaturesFromXmlFile("test-ff4j-features.xml");
        return store;
    }
}
