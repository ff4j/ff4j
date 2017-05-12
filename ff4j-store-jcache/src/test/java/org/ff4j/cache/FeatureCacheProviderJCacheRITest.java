package org.ff4j.cache;

import org.ff4j.core.Feature;
import org.ff4j.property.PropertyString;

/*
 * #%L
 * ff4j-store-ehcache
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

import org.ff4j.test.cache.AbstractCacheManagerJUnitTest;
import org.jsr107.ri.spi.RICachingProvider;
import org.junit.Assert;
import org.junit.Test;

/**
 * Test cache manager.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureCacheProviderJCacheRITest extends AbstractCacheManagerJUnitTest {

    /** {@inheritDoc} */
    protected FF4JCacheManager getCacheManager() {
        return new FF4jJCacheManager(RICachingProvider.class.getName());
    }
    
    @Test
    public void testCacheManagerProperties() {
        // Given
        FF4jJCacheManager fcm= new FF4jJCacheManager(RICachingProvider.class.getName());
        Assert.assertNotNull(fcm.getCacheProviderName());
        Assert.assertNotNull(fcm.getNativeCache());
        Assert.assertNotNull(fcm.getPropertyNativeCache());
        // When
        fcm.putProperty(new PropertyString("p1", "v1"));
        // Then
        Assert.assertNotNull(fcm.getProperty("p1"));
        Assert.assertTrue(fcm.listCachedPropertyNames().contains("p1"));
        // When 
        fcm.evictProperty("p1");
        fcm.evictProperty("p2");
        // Then
        Assert.assertNull(fcm.getProperty("p1"));
    }
    
    @Test
    public void testCacheManagerFeatures() {
        // Given
        FF4jJCacheManager fcm= new FF4jJCacheManager(RICachingProvider.class.getName());
        Assert.assertNotNull(fcm.getFeatureNativeCache());
        // When
        fcm.putFeature(new Feature("f1", true));
        // Then
        Assert.assertNotNull(fcm.getFeature("f1"));
        Assert.assertTrue(fcm.listCachedFeatureNames().contains("f1"));
        // When 
        fcm.evictFeature("f1");
        // Then
        Assert.assertNull(fcm.getFeature("p1"));
        // When
        fcm.setFeaturesCache(fcm.getFeaturesCache());
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testCacheManagerNotInitialized() {
        FF4jJCacheManager fcm= new FF4jJCacheManager();
        fcm.createCacheForFeatures();
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testCacheManagerNotInitializedBis() {
        FF4jJCacheManager fcm= new FF4jJCacheManager();
        fcm.createCacheForProperties();
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testCacheManagerNotInitializedNative() {
        FF4jJCacheManager fcm = new FF4jJCacheManager();
        fcm.getNativeCache();
    }

}
