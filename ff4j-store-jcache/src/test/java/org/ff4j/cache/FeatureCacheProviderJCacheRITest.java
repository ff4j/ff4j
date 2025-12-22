package org.ff4j.cache;

/*-
 * #%L
 * ff4j-store-jcache
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
 * ff4j-store-jcache
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

import org.ff4j.core.Feature;
import org.ff4j.property.PropertyString;

import org.ff4j.test.cache.AbstractCacheManagerJUnitTest;
import org.jsr107.ri.spi.RICachingProvider;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

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
        Assertions.assertNotNull(fcm.getCacheProviderName());
        Assertions.assertNotNull(fcm.getNativeCache());
        Assertions.assertNotNull(fcm.getPropertyNativeCache());
        // When
        fcm.putProperty(new PropertyString("p1", "v1"));
        // Then
        Assertions.assertNotNull(fcm.getProperty("p1"));
        Assertions.assertTrue(fcm.listCachedPropertyNames().contains("p1"));
        // When 
        fcm.evictProperty("p1");
        fcm.evictProperty("p2");
        // Then
        Assertions.assertNull(fcm.getProperty("p1"));
    }
    
    @Test
    public void testCacheManagerFeatures() {
        // Given
        FF4jJCacheManager fcm= new FF4jJCacheManager(RICachingProvider.class.getName());
        Assertions.assertNotNull(fcm.getFeatureNativeCache());
        // When
        fcm.putFeature(new Feature("f1", true));
        // Then
        Assertions.assertNotNull(fcm.getFeature("f1"));
        Assertions.assertTrue(fcm.listCachedFeatureNames().contains("f1"));
        // When 
        fcm.evictFeature("f1");
        // Then
        Assertions.assertNull(fcm.getFeature("p1"));
        // When
        fcm.setFeaturesCache(fcm.getFeaturesCache());
    }
    
    @Test
    public void testCacheManagerNotInitialized() {
        assertThrows(IllegalArgumentException.class, () -> {
            FF4jJCacheManager fcm = new FF4jJCacheManager();
            fcm.createCacheForFeatures();
        });
    }
    
    @Test
    public void testCacheManagerNotInitializedBis() {
        assertThrows(IllegalArgumentException.class, () -> {
            FF4jJCacheManager fcm = new FF4jJCacheManager();
            fcm.createCacheForProperties();
        });
    }
    
    @Test
    public void testCacheManagerNotInitializedNative() {
        assertThrows(IllegalArgumentException.class, () -> {
            FF4jJCacheManager fcm = new FF4jJCacheManager();
            fcm.getNativeCache();
        });
    }

}
