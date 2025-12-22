package org.ff4j.test.cache;

/*-
 * #%L
 * ff4j-core
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

import java.util.HashSet;
import java.util.Set;

import org.ff4j.FF4j;
import org.ff4j.cache.FF4JCacheManager;
import org.ff4j.cache.FF4jCacheProxy;
import org.ff4j.cache.InMemoryCacheManager;
import org.ff4j.core.Feature;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyLogLevel;
import org.ff4j.property.PropertyLogLevel.LogLevel;
import org.ff4j.property.PropertyString;
import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.store.InMemoryFeatureStore;


import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertThrows;


public class CacheProxyTest {
    
    @Test
    public void testCacheProxyNullTriggerException() {
        assertThrows(IllegalArgumentException.class, () -> {
            FF4jCacheProxy proxy = new FF4jCacheProxy();
            proxy.getTargetFeatureStore();
        });
    }
    
    @Test
    public void testCacheProxyNullTriggerException2() {
        assertThrows(IllegalArgumentException.class, () -> {
            FF4jCacheProxy proxy = new FF4jCacheProxy();
            proxy.getTargetPropertyStore();
        });
    }
    
    @Test
    public void testCacheProxyManager() {
        FF4jCacheProxy proxy = new FF4jCacheProxy();
        FF4JCacheManager cm = new InMemoryCacheManager();
        proxy.setCacheManager(cm);
        proxy.isCached();
        Assertions.assertNotNull(proxy.getCacheProvider());
        proxy.setTargetPropertyStore(new InMemoryPropertyStore());
        Assertions.assertEquals(0, proxy.readAllProperties().size());
        proxy.createProperty(new PropertyString("p1", "v1"));
        Assertions.assertTrue(proxy.existProperty("p1"));
        Assertions.assertFalse(proxy.existProperty("p2"));
        
        proxy.setTargetFeatureStore(new InMemoryFeatureStore());
        Set < Feature> setOfFeatures = new HashSet<Feature>();
        setOfFeatures.add(new Feature("f1"));
        setOfFeatures.add(new Feature("f2"));
        proxy.importFeatures(setOfFeatures);
    }
    
    @Test
    public void testCacheProxyManagerProperty() {
        FF4jCacheProxy proxy = new FF4jCacheProxy();
        proxy.setTargetPropertyStore(new InMemoryPropertyStore());
        proxy.setTargetFeatureStore(new InMemoryFeatureStore());
        proxy.setCacheManager(new InMemoryCacheManager());
        Assertions.assertTrue(proxy.isEmpty());
        
        proxy.create(new Feature("a"));
        Assertions.assertFalse(proxy.isEmpty());
        
        proxy.createProperty(new PropertyString("p1", "v1"));
        Property<?> p1 = proxy.readProperty("p1");
        proxy.readProperty("p1");
        proxy.getTargetPropertyStore().createProperty(new PropertyString("p2"));
        proxy.readProperty("p2");
        
        proxy.updateProperty("p1", "v2");
        proxy.updateProperty(p1);
        Assertions.assertFalse(proxy.isEmpty());
        
        Assertions.assertFalse(proxy.listPropertyNames().isEmpty());
        proxy.deleteProperty("p1");
        proxy.clear();
        
        Set < Property<?>> setOfProperty = new HashSet<Property<?>>();
        setOfProperty.add(new PropertyLogLevel("a", LogLevel.INFO));
        setOfProperty.add(new PropertyLogLevel("titi1", LogLevel.INFO));
        proxy.importProperties(setOfProperty);
        
        // Already in cache, but not same value
        proxy.createProperty(new PropertyString("cacheNStore", "cacheNStore"));
        proxy.readProperty("cacheNStore", p1);
        
        // Not in cache, but in store, but not same default value
        proxy.getTargetPropertyStore().createProperty(new PropertyString("p4", "v4"));
        proxy.readProperty("p1", p1);
        
        proxy.readProperty("p1", p1);
        // Nowhere, return default
        proxy.readProperty("p2", new PropertyString("p2"));
        proxy.readProperty("p1", new PropertyString("p3"));
    }
    
    @Test
    public void testCacheProxy() {
        FF4j myFF4J = new FF4j();
        Assertions.assertNull(myFF4J.getCacheProxy());
        myFF4J.setEnableAudit(true);
        Assertions.assertNull(myFF4J.getCacheProxy());
        Assertions.assertNotNull(myFF4J.getConcreteFeatureStore());
        
        
    }

}
