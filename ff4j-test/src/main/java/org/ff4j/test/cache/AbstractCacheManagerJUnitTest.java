package org.ff4j.test.cache;

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
import org.ff4j.core.Feature;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Cache manager.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public abstract class AbstractCacheManagerJUnitTest {

    public static final String DESCRIPTION = "Description";
    
    /** Cache Manager. */
    protected FF4JCacheManager cacheManager = null;
    
    /**
     * Implementation of cache manager.
     *
     * @return
     */
    protected abstract FF4JCacheManager getCacheManager();
    
    @Before
    /** Init cache. */
    public void initialize() {
        cacheManager = getCacheManager();
    }
    
    @After
    /** Clean cache. */
    public void clean() {
        getCacheManager().clearFeatures();
        getCacheManager().clearProperties();
    }
    
    /**
     * TDD.
     */
    @Test
    public void testPutOK() {
        // Given
        Feature ff = new Feature("ff", false, DESCRIPTION);
        Assert.assertFalse(getCacheManager().listCachedFeatureNames().contains(ff.getUid()));
        // When
        getCacheManager().putFeature(ff);
        // Then
        Assert.assertTrue(getCacheManager().listCachedFeatureNames().contains(ff.getUid()));
    }
    
    /**
     * TDD.
     */
    @Test
    public void testPutSeveral() {
        // Given
        Assert.assertTrue(getCacheManager().listCachedFeatureNames().isEmpty());
        // When
        getCacheManager().putFeature(new Feature("ff", false, DESCRIPTION));
        getCacheManager().putFeature(new Feature("ff2", false, DESCRIPTION));
        // Then
        Assert.assertEquals(2, getCacheManager().listCachedFeatureNames().size());
    }
   
    /**
     * TDD.
     */
    @Test
    public void testPutAvoidDoublon() {
        // Given
        Assert.assertTrue(getCacheManager().listCachedFeatureNames().isEmpty());
        // When
        getCacheManager().putFeature(new Feature("ff", false, DESCRIPTION));
        getCacheManager().putFeature(new Feature("ff", false, DESCRIPTION));
        getCacheManager().putFeature(new Feature("ff2", false, DESCRIPTION));
        // Then
        Assert.assertEquals(2, getCacheManager().listCachedFeatureNames().size());
    }
    
    /**
     * TDD.
     */
    @Test
    public void testEvictOK() {
        // Given
        getCacheManager().putFeature(new Feature("ff", false, DESCRIPTION));
        Assert.assertTrue(getCacheManager().listCachedFeatureNames().contains("ff"));
        // When
        getCacheManager().evictFeature("ff");
        // Then
        Assert.assertFalse(getCacheManager().listCachedFeatureNames().contains("ff"));
    }
    
    /**
     * TDD.
     */
    @Test
    public void testEvictFeatureNotExist() {
        // Given
        Assert.assertFalse(getCacheManager().listCachedFeatureNames().contains("ff"));
        // When
        getCacheManager().evictFeature("ff");
        // Then
        // .. everything OK...
    }
    
    /**
     * TDD.
     */
    @Test
    public void testClear() {
        // Given
        getCacheManager().putFeature(new Feature("ff", false, DESCRIPTION));
        getCacheManager().putFeature(new Feature("ff2", false, DESCRIPTION));
        getCacheManager().putFeature(new Feature("ff3", false, DESCRIPTION));
        Assert.assertEquals(3, getCacheManager().listCachedFeatureNames().size());
        // When
        getCacheManager().clearFeatures();
        // Then
        Assert.assertTrue(getCacheManager().listCachedFeatureNames().isEmpty());
    }

}
