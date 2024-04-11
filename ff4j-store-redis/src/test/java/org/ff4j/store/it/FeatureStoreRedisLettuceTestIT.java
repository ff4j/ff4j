package org.ff4j.store.it;

/*-
 * #%L
 * ff4j-store-redis
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
import org.ff4j.store.FeatureStoreRedisLettuce;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Ignore;

import io.lettuce.core.RedisClient;
import redis.clients.jedis.Protocol;

/**
 * Test to work with Redis as a store.
 */
@Ignore
public class FeatureStoreRedisLettuceTestIT extends FeatureStoreTestSupport {
   
    public static RedisClient rc = RedisClient.create("redis://localhost:" + Protocol.DEFAULT_PORT);
    
    /** {@inheritDoc} */
    @Override
    protected FeatureStore initStore() {
        FeatureStoreRedisLettuce redisStore = new FeatureStoreRedisLettuce(rc);
        redisStore.importFeaturesFromXmlFile("ff4j.xml");
        return redisStore;
    }
    
    @AfterClass
    public static void flushClient() {
        rc.shutdown();
    }
    
    /**
     * Clean store after each test (avoid duplication)
     */
    @After
    public void cleanStore() {
        testedStore.clear();
    }

}
