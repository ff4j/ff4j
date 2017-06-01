package org.ff4j.ignite;

/*
 * #%L
 * ff4j-store-jcache
 * %%
 * Copyright (C) 2013 - 2015 FF4J
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


import javax.cache.CacheManager;

import org.apache.ignite.Ignite;
import org.apache.ignite.Ignition;
import org.ff4j.cache.FF4JCacheManager;
import org.ff4j.test.cache.AbstractCacheManagerJUnitTest;
import org.junit.AfterClass;
import org.junit.BeforeClass;

/**
 * Implementation of {@link CacheManager} for feautres and HazelCast
 *
 * @author Cedrick Lunven (@clunven)</a>
 * 
 * Test are skipped as a grid is required.
 */
public class CacheManagerIgniteTest extends AbstractCacheManagerJUnitTest {
    
    /** ignite. */
    private static Ignite ignite;
    
    @BeforeClass
    public static void startIgnite() {
        ignite = Ignition.start();
    }
    
    /** {@inheritDoc} */
    protected FF4JCacheManager getCacheManager() {
        if (cacheManager == null) {
            cacheManager = new FF4jCacheManagerIgnite(ignite);
        }
        return cacheManager;
    }
    
    @AfterClass
    public static void stopIgnite() {
        if (ignite != null) {
            ignite.close();
        }
    }

}
