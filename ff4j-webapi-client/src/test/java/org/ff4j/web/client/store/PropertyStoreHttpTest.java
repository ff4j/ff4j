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

import static org.ff4j.test.TestsFf4jConstants.TEST_FEATURES_FILE;

import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;

/**
 * Run the {@link PropertyStore} TCK against {@link PropertyStoreHttp} talking to an
 * in-process HTTP server backed by an {@link InMemoryPropertyStore}.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyStoreHttpTest extends PropertyStoreTestSupport {

    /** In-process webapi. */
    private static WebApiTestServer server;

    @BeforeAll
    public static void startWebApi() throws Exception {
        server = new WebApiTestServer();
        server.start();
    }

    @AfterAll
    public static void stopWebApi() {
        server.stop();
    }

    /** {@inheritDoc} */
    @Override
    protected PropertyStore initPropertyStore() {
        server.setPropertyStore(new InMemoryPropertyStore(TEST_FEATURES_FILE));
        return new PropertyStoreHttp(server.getBaseUrl());
    }

}
