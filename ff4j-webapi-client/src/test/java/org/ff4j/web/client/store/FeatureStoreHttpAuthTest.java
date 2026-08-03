package org.ff4j.web.client.store;

/*-
 * #%L
 * ff4j-webapi-client
 * %%
 * Copyright (C) 2013 - 2026 FF4J
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

import static org.ff4j.test.TestsFf4jConstants.F1;
import static org.ff4j.test.TestsFf4jConstants.TEST_FEATURES_FILE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.ff4j.exception.FeatureAccessException;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.web.client.utils.ClientHttpUtils;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

/**
 * Check the {@code Authorization} header propagation (apiKey and user/password flavors).
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class FeatureStoreHttpAuthTest {

    /** Sample api key. */
    private static final String API_KEY = "my-api-key-20260802";

    /** In-process webapi. */
    private static WebApiTestServer server;

    @BeforeAll
    public static void startWebApi() throws Exception {
        server = new WebApiTestServer();
        server.start();
        server.setFeatureStore(new InMemoryFeatureStore(TEST_FEATURES_FILE));
        server.setExpectedAuthorization(ClientHttpUtils.buildAuthorization4ApiKey(API_KEY));
    }

    @AfterAll
    public static void stopWebApi() {
        server.stop();
    }

    @Test
    public void shouldReadFeatureWithValidApiKey() {
        FeatureStoreHttp store = new FeatureStoreHttp(server.getBaseUrl(), API_KEY);
        assertTrue(store.exist(F1));
        assertEquals(F1, store.read(F1).getUid());
    }

    @Test
    public void shouldFailWithoutApiKey() {
        FeatureStoreHttp store = new FeatureStoreHttp(server.getBaseUrl());
        assertThrows(FeatureAccessException.class, () -> store.read(F1));
    }

    @Test
    public void shouldFailWithWrongApiKey() {
        FeatureStoreHttp store = new FeatureStoreHttp(server.getBaseUrl(), "wrong-key");
        assertThrows(FeatureAccessException.class, () -> store.exist(F1));
    }

}
