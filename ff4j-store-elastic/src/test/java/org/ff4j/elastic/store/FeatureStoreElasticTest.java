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

import org.ff4j.core.FeatureStore;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.junit.Ignore;

/**
 * Fixing for ES 6+.
 *
 * @author Cedrick LUNVEN (@clunven)
 * @author <a href="mailto:andre.blaszczyk@gmail.com">Andre Blaszczyk</a>
 */
@Ignore
public class FeatureStoreElasticTest extends FeatureStoreTestSupport {
   
	/** {@inheritDoc} */
	@Override
	protected FeatureStore initStore() {
	    FeatureStoreElastic fse = new FeatureStoreElastic(
	            JestClientTestFactory.getJestClient(), 
	            FeatureStoreElastic.DEFAULT_INDEX_FEATURES);
	    // Initialize the store we only what we need to test
	    fse.clear();
	    fse.importFeaturesFromXmlFile("test-ff4j-features.xml");
	    return fse;
	}
}
