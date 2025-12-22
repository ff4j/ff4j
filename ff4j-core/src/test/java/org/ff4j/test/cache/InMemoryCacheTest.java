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

import static org.junit.jupiter.api.Assertions.assertThrows;

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

import org.ff4j.cache.FF4jCacheProxy;
import org.ff4j.cache.InMemoryCacheManager;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.property.PropertyString;
import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.test.store.CoreFeatureStoreTestSupport;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

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
        Assertions.assertNotNull(fcm.getFeatureNativeCache());
        Assertions.assertNotNull(fcm.getPropertyNativeCache());
    }

    @Test
    public void testPutNullisIlegal() {
        assertThrows(IllegalArgumentException.class, () ->
            new InMemoryCacheManager().putFeature(null));
    }

    @Test
    public void testPutNullisIlegal2() {
        assertThrows(IllegalArgumentException.class, () ->
            new InMemoryCacheManager().putFeature(null, 1));
    }
    
    @Test
    public void testPutNullPropertyisIlegal() {
        assertThrows(IllegalArgumentException.class, () ->
            new InMemoryCacheManager().putProperty(null));
    }

    @Test
    public void testPutNullFeatureId() {
        assertThrows(IllegalArgumentException.class, () -> {
            Feature f = new Feature("a");
            f.setUid(null);
            new InMemoryCacheManager().putFeature(f);
        });
    }
    
    @Test
    public void testPutNullFeatureId1() {
        assertThrows(IllegalArgumentException.class, () -> {
            Feature f = new Feature("a");
            f.setUid(null);
            new InMemoryCacheManager().putFeature(f, 1);
        });
    }
    
    @Test
    public void testPutNullPropertyName() {
        assertThrows(IllegalArgumentException.class, () -> {
            PropertyString p = new PropertyString();
            p.setName(null);
            new InMemoryCacheManager().putProperty(p);
        });
    }

    @Test
    public void testPutEmptyFeatureId() {
        assertThrows(IllegalArgumentException.class, () -> {
            Feature f = new Feature("a");
            f.setUid("");
            new InMemoryCacheManager().putFeature(f);
        });
    }
    
    
    @Test
    public void testPutEmptyPropertyName() {
        assertThrows(IllegalArgumentException.class, () -> {
            PropertyString p = new PropertyString();
            p.setName("");
            new InMemoryCacheManager().putProperty(p);
        });
    }

    @Test
    public void testRequiredArgumentCacheManager() {
        assertThrows(IllegalArgumentException.class, () ->
            new FF4jCacheProxy().getCacheManager());
    }

    @Test
    public void testExistBis() {
        FF4jCacheProxy fscp = new FF4jCacheProxy(
                new InMemoryFeatureStore("ff4j.xml"), null,  
                new InMemoryCacheManager());
        Assertions.assertFalse(fscp.exist("toto"));
        Assertions.assertFalse(fscp.exist("toto"));
        Assertions.assertTrue(fscp.exist("first"));
        Assertions.assertTrue(fscp.exist("first"));
    }
    
    @Test
    public void testClear() {
        // Given
        InMemoryCacheManager imcm = new InMemoryCacheManager();
        imcm.putProperty(new PropertyString("p1"));
        Assertions.assertFalse(imcm.listCachedPropertyNames().isEmpty());
        // When
        imcm.clearProperties();
        // Then
        Assertions.assertTrue(imcm.listCachedPropertyNames().isEmpty());
    }
    
    @Test
    public void testEvictProperty1() {
        // Given
        InMemoryCacheManager imcm = new InMemoryCacheManager();
        imcm.putProperty(new PropertyString("p1"));
        Assertions.assertFalse(imcm.listCachedPropertyNames().isEmpty());
        // When
        imcm.evictProperty("p1");
        // Then
        Assertions.assertTrue(imcm.listCachedPropertyNames().isEmpty());
    }
    
    @Test
    public void testEvictProperty2() {
        // Given
        InMemoryCacheManager imcm = new InMemoryCacheManager();
        imcm.putProperty(new PropertyString("p2"));
        Assertions.assertFalse(imcm.listCachedPropertyNames().isEmpty());
        // When
        imcm.evictProperty("p1");
        // Then
        Assertions.assertFalse(imcm.listCachedPropertyNames().isEmpty());
    }
    
    @Test
    public void testReadFeature() {
        // Given
        InMemoryCacheManager imcm = new InMemoryCacheManager();
        imcm.putFeature(new Feature("f1"), 100);
        // When
        Feature f = imcm.getFeature("f1");
        // Then
        Assertions.assertNotNull(f);
        // When
        imcm.putFeature(new Feature("f1"), 1);
        
    }
    
    @Test
    public void testAccessors() {
        InMemoryCacheManager imcm = new InMemoryCacheManager();
        Assertions.assertNotNull(imcm.getCacheProviderName());
        Assertions.assertTrue(imcm.listCachedFeatureNames().isEmpty());
        Assertions.assertTrue(imcm.listCachedPropertyNames().isEmpty());
    }
    
    @Test
    public void testGetProperty() throws InterruptedException {
        InMemoryCacheManager imcm = new InMemoryCacheManager();
        imcm.putProperty(new PropertyString("p1"));
        Assertions.assertNull(imcm.getProperty("p2"));
        Assertions.assertNotNull(imcm.getProperty("p1"));
    }

    @Test
    public void testGetFeatureTimeout() throws InterruptedException {
        InMemoryCacheManager imcm = new InMemoryCacheManager();
        imcm.putFeature(new Feature("f2"), 1);
        Thread.sleep(1100);
        Assertions.assertNull(imcm.getFeature("f2"));
    }
    
    @Test
    public void testGetPropertyTimeout() throws InterruptedException {
        InMemoryCacheManager imcm = new InMemoryCacheManager();
        imcm.putProperty(new PropertyString("p1"), 1);
        imcm.putProperty(new PropertyString("p2"), 10);
        Thread.sleep(1100);
        Assertions.assertNull(imcm.getProperty("p1"));
        Assertions.assertNotNull(imcm.getProperty("p2"));
    }
    
    @Test
    public void testGetProperty2() {
        assertThrows(IllegalArgumentException.class, () -> {
            InMemoryCacheManager imcm = new InMemoryCacheManager();
            imcm.putProperty(null, 1);
        });
    }
    
    @Test
    public void testGetProperty3() {
        assertThrows(IllegalArgumentException.class, () -> {
            InMemoryCacheManager imcm = new InMemoryCacheManager();
            PropertyString p1 = new PropertyString("p1");
            p1.setName(null);
            imcm.putProperty(p1, 1);
        });
    }
    
    @Test
    public void testGetProperty4() {
        assertThrows(IllegalArgumentException.class, () -> {
            InMemoryCacheManager imcm = new InMemoryCacheManager();
            PropertyString p1 = new PropertyString("p1");
            p1.setName("");
            imcm.putProperty(p1, 1);
        });
    }
   
}
