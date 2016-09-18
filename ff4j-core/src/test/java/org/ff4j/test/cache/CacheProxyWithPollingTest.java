package org.ff4j.test.cache;

import org.ff4j.cache.FF4JCacheManager;
import org.ff4j.cache.FF4jCacheProxy;
import org.ff4j.cache.InMemoryCacheManager;
import org.ff4j.core.FeatureStore;
import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.store.InMemoryFeatureStore;
import org.junit.Assert;
import org.junit.Test;

public class CacheProxyWithPollingTest {
    
    @Test
    public void testCacheProxyManagerProperty() throws InterruptedException {
        // When
        FeatureStore  fs = new InMemoryFeatureStore("ff4j.xml");
        PropertyStore ps = new InMemoryPropertyStore("ff4j.xml");
        FF4JCacheManager cm = new InMemoryCacheManager();
        FF4jCacheProxy proxy = new FF4jCacheProxy(fs, ps, cm);
        // Start polling on 100ms basis
        proxy.startPolling(100);
        Thread.sleep(200);
        
        // When (Remove something)
        fs.delete("AwesomeFeature");
        // Then (Proxy is not yet refresh)
        Assert.assertTrue(proxy.exist("AwesomeFeature"));
        
        // When (wait for cache refresh)
        Thread.sleep(200);
        // Then (also delete in cache si Cache is refreshed)
        Assert.assertFalse(proxy.exist("AwesomeFeature"));
    }
    
    

}
