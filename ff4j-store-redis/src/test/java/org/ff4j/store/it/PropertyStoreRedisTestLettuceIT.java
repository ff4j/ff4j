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


import org.ff4j.property.store.PropertyStore;
import org.ff4j.store.PropertyStoreRedisLettuce;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Ignore;

import io.lettuce.core.RedisClient;

/**
 * Implementatino of tests with REDIS
 * @author Cedrick Lunven (@clunven)</a>
 */
@Ignore
public class PropertyStoreRedisTestLettuceIT extends PropertyStoreTestSupport {

    public static RedisClient rc = RedisClient.create("redis://localhost");
    
    /** {@inheritDoc} */
    protected PropertyStore initPropertyStore() {
        PropertyStoreRedisLettuce prl =  new PropertyStoreRedisLettuce(rc);
        prl.importPropertiesFromXmlFile("test-ff4j-features.xml");
        return prl;
    }
    
    /**
     * Clean store after each test (avoid duplication)
     */
    @After
    public void cleanStore() {
        testedStore.clear();
    }
    
    @AfterClass
    public static void flushClient() {
        rc.shutdown();
    }

}
