package org.ff4j.hazelcast;

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
import org.ff4j.cache.FF4JCacheManager;
import org.ff4j.test.cache.AbstractCacheManagerJUnitTest;
import org.junit.BeforeClass;

/**
 * Implementation of {@link CacheManager} for feautres and HazelCast
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class CacheManagerHazelCastTest extends AbstractCacheManagerJUnitTest {

    private static CacheManagerHazelCast cacheManagerHazelCast;

    @BeforeClass
    public static void setupHazelcast() {
        cacheManagerHazelCast = new CacheManagerHazelCast();
    }

    /**
     * {@inheritDoc}
     */
    protected FF4JCacheManager getCacheManager() {
        return cacheManagerHazelCast;
    }

}
