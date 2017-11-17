package org.ff4j.cache;

/*
 * #%L
 * ff4j-store-ehcache
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class EhCacheCacheManagerTest2 {

    private FF4j ff4j = null;
    
    private FF4JCacheManager cacheManager = null; 
    
    @Before
    /** Init cache. */
    public void initialize() {
        cacheManager = new FeatureCacheProviderEhCache();
        
        // Stores in memory
        ff4j = new FF4j();
        ff4j.createFeature(new Feature("f1", false));
        
        // Enable caching using EHCACHE
        ff4j.cache(cacheManager);
        
        // How to access cacheManager from FF4J
        // ff4j.getCacheProxy().getCacheManager();
    }
    
    @Test
    public void testPlayingWithCache() {
        // Update with Check
        Assert.assertFalse(cacheManager.listCachedFeatureNames().contains("f1"));
        ff4j.check("f1");
        Assert.assertTrue(cacheManager.listCachedFeatureNames().contains("f1"));
        
        // Updated for create/update
        Assert.assertFalse(cacheManager.listCachedFeatureNames().contains("f3"));
        ff4j.createFeature(new Feature("f3", false));
        Assert.assertTrue(cacheManager.listCachedFeatureNames().contains("f3"));
        ff4j.enable("f3");
        // Is cache also updated ?
        Assert.assertFalse(cacheManager.listCachedFeatureNames().contains("f3"));
        
        // Updated for deletion
        ff4j.check("f3");
        Assert.assertTrue(cacheManager.listCachedFeatureNames().contains("f3"));
        ff4j.delete("f3");
        Assert.assertFalse(cacheManager.listCachedFeatureNames().contains("f3"));
        
    }
    
    @After
    public void clearCache() {
        ff4j.getCacheProxy().clear();
    }
    
    
}
