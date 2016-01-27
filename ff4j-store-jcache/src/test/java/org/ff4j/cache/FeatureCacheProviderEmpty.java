package org.ff4j.cache;

import org.jsr107.ri.spi.RICachingProvider;
import org.junit.Assert;

/*
 * #%L
 * ff4j-store-jcache
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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


import org.junit.Test;

public class FeatureCacheProviderEmpty {

    @Test(expected = IllegalArgumentException.class)
    public void initCacheManager() {
        new FF4jJCacheManager(null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void initCacheProvider() {
        new FF4jJCacheProvider(null);
    }
    
    @Test
    public void initCacheProviderBis() {
        FF4jJCacheProvider fcp = new FF4jJCacheProvider(RICachingProvider.class.getName());
        Assert.assertNull(fcp.initCachingProvider(null));
    }
    
}
