package org.ff4j.gcpdatastore.store;

import lombok.extern.slf4j.Slf4j;
import org.ff4j.core.FeatureStore;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.junit.ClassRule;

/*
 * #%L
 * ff4j-store-arangodb
 * %%
 * Copyright (C) 2022 FF4J
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

@Slf4j
public class GcpDatastoreFeatureStoreTest extends FeatureStoreTestSupport {

    @ClassRule
    public static GcpDatastoreTestContainer container = new GcpDatastoreTestContainer();

    @Override
    protected FeatureStore initStore() {
        log.info("Container port:" + container.getExposedPorts());
        return null;
    }
}
