package org.ff4j.test.cache;

import org.ff4j.cache.FF4JCacheManager;
import org.ff4j.cache.FF4jCacheProxy;
import org.ff4j.cache.InMemoryCacheManager;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyString;
import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.store.InMemoryFeatureStore;
import org.junit.Assert;

/*
 * #%L
 * ff4j-core
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


public class CacheProxyTest {
    
    @Test(expected = IllegalArgumentException.class)
    public void testCacheProxyNullTriggerException() {
        FF4jCacheProxy proxy = new FF4jCacheProxy();
        proxy.getTargetFeatureStore();
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testCacheProxyNullTriggerException2() {
        FF4jCacheProxy proxy = new FF4jCacheProxy();
        proxy.getTargetPropertyStore();
    }
    
    @Test
    public void testCacheProxyManager() {
        FF4jCacheProxy proxy = new FF4jCacheProxy();
        FF4JCacheManager cm = new InMemoryCacheManager();
        proxy.setCacheManager(cm);
        proxy.isCached();
        Assert.assertNotNull(proxy.getCacheProvider());
        proxy.setTargetPropertyStore(new InMemoryPropertyStore());
        Assert.assertEquals(0, proxy.readAllProperties().size());
        proxy.createProperty(new PropertyString("p1", "v1"));
        Assert.assertTrue(proxy.existProperty("p1"));
        Assert.assertFalse(proxy.existProperty("p2"));
    }
    
    @Test
    public void testCacheProxyManagerProperty() {
        FF4jCacheProxy proxy = new FF4jCacheProxy();
        proxy.setTargetPropertyStore(new InMemoryPropertyStore());
        proxy.setTargetFeatureStore(new InMemoryFeatureStore());
        proxy.setCacheManager(new InMemoryCacheManager());
        proxy.createProperty(new PropertyString("p1", "v1"));
       
        Property<?> p1 = proxy.readProperty("p1");
        proxy.readProperty("p1");
        proxy.getTargetPropertyStore().createProperty(new PropertyString("p2"));
        proxy.readProperty("p2");
        
        
        proxy.updateProperty("p1", "v2");
        proxy.updateProperty(p1);
        Assert.assertTrue(proxy.isEmpty());
        Assert.assertFalse(proxy.listPropertyNames().isEmpty());
        proxy.deleteProperty("p1");
        proxy.clear();
    }

}
