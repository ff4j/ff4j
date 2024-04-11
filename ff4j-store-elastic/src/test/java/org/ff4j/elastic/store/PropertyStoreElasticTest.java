package org.ff4j.elastic.store;

/*-
 * #%L
 * ff4j-store-elastic
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

import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.Ignore;

/**
 * Testing property Store.
 *
 * @author Cedrick LUNVEN (@clunven)
 * @author <a href="mailto:andre.blaszczyk@gmail.com">Andre Blaszczyk</a>
 */
@Ignore
public class PropertyStoreElasticTest extends PropertyStoreTestSupport {
   
    /** {@inheritDoc} */
    @Override
    protected PropertyStore initPropertyStore() {
        PropertyStoreElastic pse = new PropertyStoreElastic(
                JestClientTestFactory.getJestClient(), 
                PropertyStoreElastic.DEFAULT_INDEX_PROPERTIES);
        // Initialize the store we only what we need to test
        pse.clear();
        pse.importPropertiesFromXmlFile("test-ff4j-features.xml");
        return pse;
    }

}
