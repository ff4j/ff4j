package org.ff4j.store.it;

/*
 * #%L
 * ff4j-store-redis
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

import java.util.Map;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.store.FeatureStoreRedis;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.junit.After;

/**
 * Test to work with Redis as a store.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreRedisTestIT extends FeatureStoreTestSupport {
   
    /** {@inheritDoc} */
    @Override
    protected FeatureStore initStore() {
        FeatureStoreRedis redisStore = new FeatureStoreRedis();
        redisStore.importFeaturesFromXmlFile("ff4j.xml");
        return redisStore;
    }
    
    /**
     * Clean store after each test (avoid duplication)
     */
    @After
    public void cleanStore() {
        Map < String, Feature > f = testedStore.readAll();
        for (String key : f.keySet()) {
            testedStore.delete(key);
        }
        // Close Pool
        ((FeatureStoreRedis) ff4j.getConcreteFeatureStore()).getRedisConnection().destroyPool();
    }

}
