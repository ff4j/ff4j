package org.ff4j.test.store;

/*
 * #%L
 * ff4j-test
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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

import org.ff4j.cache.FF4JCacheManager;
import org.ff4j.cache.InMemoryCacheManager;
import org.ff4j.test.cache.AbstractCacheManagerJUnitTest;

/**
 * Test for cache manager in memory.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class InMemoryFeatureCache extends AbstractCacheManagerJUnitTest {

    private FF4JCacheManager cacheManager = new InMemoryCacheManager();
    
    /** {@inheritDoc} */
    @Override
    protected FF4JCacheManager getCacheManager() {
        return cacheManager;
    }

}
