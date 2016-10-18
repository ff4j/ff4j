package org.ff4j.test.cache;

import org.ff4j.cache.FF4jCacheProxy;
import org.ff4j.cache.InMemoryCacheManager;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.property.PropertyString;
import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.test.store.CoreFeatureStoreTestSupport;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 Ff4J
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

import org.junit.Assert;
import org.junit.Test;

/**
 * Testing class of {@link InMemoryCacheManager} class.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class InMemoryCacheTest extends CoreFeatureStoreTestSupport {

    /** {@inheritDoc} */
    @Override
    public FeatureStore initStore() {
        return new FF4jCacheProxy(
                new InMemoryFeatureStore("ff4j.xml"), 
                new InMemoryPropertyStore("ff4j.xml"),
                new InMemoryCacheManager());
    }

    @Test
    public void testInitializations() {
        InMemoryCacheManager fcm = new InMemoryCacheManager();        
        Assert.assertNotNull(fcm.getFeatureNativeCache());
        Assert.assertNotNull(fcm.getPropertyNativeCache());
    }

    @Test(expected = IllegalArgumentException.class)
    public void testPutNullisIlegal() {
        new InMemoryCacheManager().putFeature(null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testPutNullisIlegal2() {
        new InMemoryCacheManager().putFeature(null, 1);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testPutNullPropertyisIlegal() {
        new InMemoryCacheManager().putProperty(null);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testPutNullFeatureId() {
        Feature f = new Feature("a");
        f.setUid(null);
        new InMemoryCacheManager().putFeature(f);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testPutNullFeatureId1() {
        Feature f = new Feature("a");
        f.setUid(null);
        new InMemoryCacheManager().putFeature(f,1);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testPutNullPropertyName() {
        PropertyString p = new PropertyString();
        p.setName(null);
        new InMemoryCacheManager().putProperty(p);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testPutEmptyFeatureId() {
        Feature f = new Feature("a");
        f.setUid("");
        new InMemoryCacheManager().putFeature(f);
    }
    
    
    @Test(expected = IllegalArgumentException.class)
    public void testPutEmptyPropertyName() {
        PropertyString p = new PropertyString();
        p.setName("");
        new InMemoryCacheManager().putProperty(p);
    }

    @Test(expected = IllegalArgumentException.class)
    public void testRequiredArgumentCacheManager() {
        new FF4jCacheProxy().getCacheManager();
    }

    @Test
    public void testExistBis() {
        FF4jCacheProxy fscp = new FF4jCacheProxy(
                new InMemoryFeatureStore("ff4j.xml"), null,  
                new InMemoryCacheManager());
        Assert.assertFalse(fscp.exist("toto"));
        Assert.assertFalse(fscp.exist("toto"));
        Assert.assertTrue(fscp.exist("first"));
        Assert.assertTrue(fscp.exist("first"));
    }
    
    @Test
    public void testClear() {
        // Given
        InMemoryCacheManager imcm = new InMemoryCacheManager();
        imcm.putProperty(new PropertyString("p1"));
        Assert.assertFalse(imcm.listCachedPropertyNames().isEmpty());
        // When
        imcm.clearProperties();
        // Then
        Assert.assertTrue(imcm.listCachedPropertyNames().isEmpty());
    }
    
    @Test
    public void testEvictProperty1() {
        // Given
        InMemoryCacheManager imcm = new InMemoryCacheManager();
        imcm.putProperty(new PropertyString("p1"));
        Assert.assertFalse(imcm.listCachedPropertyNames().isEmpty());
        // When
        imcm.evictProperty("p1");
        // Then
        Assert.assertTrue(imcm.listCachedPropertyNames().isEmpty());
    }
    
    @Test
    public void testEvictProperty2() {
        // Given
        InMemoryCacheManager imcm = new InMemoryCacheManager();
        imcm.putProperty(new PropertyString("p2"));
        Assert.assertFalse(imcm.listCachedPropertyNames().isEmpty());
        // When
        imcm.evictProperty("p1");
        // Then
        Assert.assertFalse(imcm.listCachedPropertyNames().isEmpty());
    }
    
    @Test
    public void testReadFeature() {
        // Given
        InMemoryCacheManager imcm = new InMemoryCacheManager();
        imcm.putFeature(new Feature("f1"), 100);
        // When
        Feature f = imcm.getFeature("f1");
        // Then
        Assert.assertNotNull(f);
        // When
        imcm.putFeature(new Feature("f1"), 1);
        
    }
    
    @Test
    public void testAccessors() {
        InMemoryCacheManager imcm = new InMemoryCacheManager();
        Assert.assertNotNull(imcm.getCacheProviderName());
        Assert.assertTrue(imcm.listCachedFeatureNames().isEmpty());
        Assert.assertTrue(imcm.listCachedPropertyNames().isEmpty());
    }
    
    @Test
    public void testGetProperty() throws InterruptedException {
        InMemoryCacheManager imcm = new InMemoryCacheManager();
        imcm.putProperty(new PropertyString("p1"));
        Assert.assertNull(imcm.getProperty("p2"));
        Assert.assertNotNull(imcm.getProperty("p1"));
    }

    @Test
    public void testGetFeatureTimeout() throws InterruptedException {
        InMemoryCacheManager imcm = new InMemoryCacheManager();
        imcm.putFeature(new Feature("f2"), 1);
        Thread.sleep(1100);
        Assert.assertNull(imcm.getFeature("f2"));
    }
    
    @Test
    public void testGetPropertyTimeout() throws InterruptedException {
        InMemoryCacheManager imcm = new InMemoryCacheManager();
        imcm.putProperty(new PropertyString("p1"), 1);
        imcm.putProperty(new PropertyString("p2"), 10);
        Thread.sleep(1100);
        Assert.assertNull(imcm.getProperty("p1"));
        Assert.assertNotNull(imcm.getProperty("p2"));
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testGetProperty2()  {
        InMemoryCacheManager imcm = new InMemoryCacheManager();
        imcm.putProperty(null, 1);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testGetProperty3()  {
        InMemoryCacheManager imcm = new InMemoryCacheManager();
        PropertyString p1 = new PropertyString("p1");
        p1.setName(null);
        imcm.putProperty(p1, 1);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testGetProperty4()  {
        InMemoryCacheManager imcm = new InMemoryCacheManager();
        PropertyString p1 = new PropertyString("p1");
        p1.setName("");
        imcm.putProperty(p1, 1);
    }
   
}
