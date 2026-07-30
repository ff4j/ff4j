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
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.gcpdatastore.store.feature.DatastoreFeatureStore;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import static org.ff4j.test.TestsFf4jConstants.*;

public class DatastoreFeatureStoreTest extends FeatureStoreTestSupport {

    private static final long QUERY_CONSISTENCY_TIMEOUT_MILLIS = 5_000;

    public static DatastoreTestContainer container = new DatastoreTestContainer();

    @BeforeClass
    public static void startContainer() {
        container.start();
    }

    @AfterClass
    public static void stopContainer() {
        container.stop();
    }

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

    @Override
    @Test
    public void testAddFeature() throws Exception {
        assertFf4j.assertThatFeatureDoesNotExist(FEATURE_NEW);

        Set<String> rights = new HashSet<>(Collections.singletonList(ROLE_USER));
        Feature feature = new Feature(FEATURE_NEW, true, "description", G1, rights);
        testedStore.create(feature);

        assertStoreEventuallyHasSize(EXPECTED_FEATURES_NUMBERS + 1);
        assertFf4j.assertThatFeatureExist(FEATURE_NEW);
        assertFf4j.assertThatFeatureIsInGroup(FEATURE_NEW, G1);

        testedStore.delete(FEATURE_NEW);
        assertFf4j.assertThatFeatureDoesNotExist(FEATURE_NEW);
    }

    private void assertStoreEventuallyHasSize(int expectedSize) throws InterruptedException {
        // The Datastore emulator can expose a key lookup before the corresponding kind query catches up.
        long deadline = System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(QUERY_CONSISTENCY_TIMEOUT_MILLIS);
        int actualSize;
        do {
            actualSize = testedStore.readAll().size();
            if (actualSize == expectedSize) {
                return;
            }
            Thread.sleep(100);
        } while (System.nanoTime() < deadline);

        Assert.assertEquals(expectedSize, actualSize);
    }
}
