package org.ff4j.test.store;

/*
 * #%L
 * ff4j-core
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
import org.ff4j.store.RESTFeatureStore;
import java.net.MalformedURLException;
import java.net.URL;

/**
 * Test the RESTFeatureStore, needs a valid FF4J rest api to be available, or mocked in test
 */
public class RESTFeatureStoreTest extends CoreFeatureStoreTestSupport {
    @Override
    protected FeatureStore initStore() {
        RESTFeatureStore restFeatureStore = null;

        try {
            URL mockRestURL = new URL("http://localhost:8080");
            restFeatureStore = new RESTFeatureStore(mockRestURL);
        } catch (MalformedURLException e) {
            System.err.println(e.getMessage());
        }

        return restFeatureStore;
    }
}
