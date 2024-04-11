package org.ff4j.parser.yaml;

/*-
 * #%L
 * ff4j-config-yaml
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
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.test.store.FeatureStoreTestSupport;

/**
 * All TEST LOGIC is in super class to be processed on EACH STORE.
 * 
 * @author clunven
 */
public class InMemoryFeatureStoreYamlTest extends FeatureStoreTestSupport {

    /** {@inheritDoc} */
    @Override
    public FeatureStore initStore() {
        return  new InMemoryFeatureStore(
                new YamlParser().parseConfigurationFile(
                        getClass().getClassLoader()
                                  .getResourceAsStream("test-ff4j-features.yml")));
    }

}
