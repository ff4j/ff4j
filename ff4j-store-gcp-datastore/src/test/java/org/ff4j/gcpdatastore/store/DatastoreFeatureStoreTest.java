package org.ff4j.gcpdatastore.store;

/*-
 * #%L
 * ff4j-store-gcp-datastore
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

import com.google.cloud.datastore.Datastore;
import com.google.cloud.datastore.DatastoreOptions;
import org.ff4j.core.FeatureStore;
import org.ff4j.gcpdatastore.store.feature.DatastoreFeatureStore;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.junit.ClassRule;
import org.junit.Test;

public class DatastoreFeatureStoreTest extends FeatureStoreTestSupport {

    @ClassRule
    public static DatastoreTestContainer container = new DatastoreTestContainer();

    @Override
    protected FeatureStore initStore() {
        Integer port = container.getFirstMappedPort();
        Datastore datastore = DatastoreOptions.newBuilder()
                .setHost("localhost:" + port)
                .setProjectId("test")
                .build()
                .getService();

        DatastoreFeatureStore store = new DatastoreFeatureStore(datastore);
        store.clear();
        store.importFeaturesFromXmlFile("test-ff4j-features.xml");
        return store;
    }

    @Test
    public void testStoreHasBeenInitialized() {}
    //@Test
    //public void testAddFeature() throws Exception {
        // overriding
    //}
}
