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

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import org.ff4j.cache.FF4JCacheManager;
import org.ff4j.cache.FF4jCacheProxy;
import org.ff4j.cache.InMemoryCacheManager;
import org.ff4j.cache.Store2CachePollingScheduler;
import org.ff4j.core.FeatureStore;
import org.ff4j.property.Property;
import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.store.InMemoryFeatureStore;
import org.junit.Assert;
import org.junit.Test;

public class CacheProxyWithPollingTest {
    
    @Test
    public void testCacheProxyManagerProperty() throws InterruptedException {
        // When
        FeatureStore  fs     = new InMemoryFeatureStore("ff4j.xml");
        PropertyStore ps     = new InMemoryPropertyStore("ff4j.xml");
        FF4JCacheManager cm  = new InMemoryCacheManager();
        FF4jCacheProxy proxy = new FF4jCacheProxy(fs, ps, cm);

        // Start polling on 100ms basis
        proxy.startPolling(100);
        proxy.createSchema();
        Thread.sleep(200);
        
        // When (Remove something)
        fs.delete("AwesomeFeature");
        // Then (Proxy is not yet refresh)
        Assert.assertTrue(proxy.exist("AwesomeFeature"));
        
        // When (wait for cache refresh)
        Thread.sleep(200);
        // Then (also delete in cache si Cache is refreshed)
        Assert.assertFalse(proxy.exist("AwesomeFeature"));
        
        Store2CachePollingScheduler scheduler = proxy.getStore2CachePoller();
        scheduler.setInitialDelay(scheduler.getInitialDelay());
        scheduler.setPollingDelay(scheduler.getPollingDelay());
        proxy.stopPolling();
        
        proxy.setStore2CachePoller(new Store2CachePollingScheduler(fs, ps, cm));
    }
    
    
    @Test(expected = IllegalStateException.class)
    public void testStartCacheProxy() {
        FF4jCacheProxy proxy = new FF4jCacheProxy();
        proxy.startPolling(100);
    }
    
    @Test(expected = IllegalStateException.class)
    public void testStopCacheProxy() {
        FF4jCacheProxy proxy = new FF4jCacheProxy();
        proxy.stopPolling();
    }


    @Test
    public void testCacheProxyManagerPropertyDuringRefresh() throws InterruptedException {

        FeatureStore fs = new InMemoryFeatureStore("ff4j.xml");
        PropertyStore ps = new InMemoryPropertyStore("ff4j.xml");
        FF4JCacheManager cm = new InMemoryCacheManager();
        final FF4jCacheProxy proxy = new FF4jCacheProxy(fs, ps, cm);

        //scheduler refreshing cache through worker thread
        Store2CachePollingScheduler store2CachePollingScheduler = new Store2CachePollingScheduler(proxy);
        // setting polling delay 10ms
        store2CachePollingScheduler.start(10);

        // 20 threads trying to fetch property from cacheManager
        ExecutorService fetchPropertyService  = Executors.newFixedThreadPool(20);

        Callable<Property<?>> callable = new Callable<Property<?>>() {
            @Override
            public Property<?> call() throws Exception {
                try {
                    return proxy.getCacheManager().getProperty("a");
                } catch (Exception e) {
                    throw e;
                }

            }
        };

        List<Callable<Property<?>>> multiplePropertyFetchCalls = new ArrayList<Callable<Property<?>>>(1000);
        //generating 100000 requests
        for (int i = 0; i < 100; i++) {
            for (int j = 0; j < 10; j++) {
                //clear to avoid accumulation of callables over multiple iterations
                multiplePropertyFetchCalls.clear();
            }
            for (int k = 0; k < 100; k++) {
                multiplePropertyFetchCalls.add(callable);
            }
            //execute 100 property fetch calls on 20 threads
            List<Future<Property<?>>> fetchPropertyCalls = fetchPropertyService.invokeAll(multiplePropertyFetchCalls);
            //property should never be null
            for (Future<Property<?>> property : fetchPropertyCalls) {
                Assert.assertNotNull(property);
            }

        }
    }
    
    

}
