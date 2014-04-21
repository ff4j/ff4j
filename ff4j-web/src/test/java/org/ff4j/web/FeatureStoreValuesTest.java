package org.ff4j.web;

/*
 * #%L
 * ff4j-web
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.test.AbstractStoreTest;

/**
 * Test that sotre informations are correct.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreValuesTest extends AbstractStoreTest {

    /** {@inheritDoc} */
    @Override
    protected FeatureStore initStore() {
        return new InMemoryFeatureStore("test-WebApi-ff4j.xml");
    }

}
