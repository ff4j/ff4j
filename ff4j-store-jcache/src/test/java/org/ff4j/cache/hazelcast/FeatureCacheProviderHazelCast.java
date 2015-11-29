package org.ff4j.cache.hazelcast;

import java.util.Random;

import javax.cache.CacheManager;
import javax.cache.spi.CachingProvider;

import org.ff4j.cache.FF4jJCacheManager;

import com.hazelcast.cache.impl.HazelcastServerCachingProvider;
import com.hazelcast.config.Config;
import com.hazelcast.config.XmlConfigBuilder;
import com.hazelcast.core.Hazelcast;
import com.hazelcast.core.HazelcastInstance;

/**
 * Implementation of {@link CacheManager} for feautres and HazelCast
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FeatureCacheProviderHazelCast extends FF4jJCacheManager {
    
    /** Internal HazelCast Settings. */
    private HazelcastInstance instance;
    
    /** {@inheritDoc} */
    @Override
    public CachingProvider initCachingProvider(String className) {
        System.setProperty("hazelcast.version.check.enabled", "false");
        System.setProperty("hazelcast.mancenter.enabled", "false");
        System.setProperty("hazelcast.wait.seconds.before.join", "1");
        System.setProperty("hazelcast.local.localAddress", "127.0.0.1");
        System.setProperty("java.net.preferIPv4Stack", "true");
        System.setProperty("hazelcast.jmx", "true");
        Random rand = new Random();
        int g1 = rand.nextInt(255);
        int g2 = rand.nextInt(255);
        int g3 = rand.nextInt(255);
        System.setProperty("hazelcast.multicast.group", "224." + g1 + "." + g2 + "." + g3);
        System.setProperty("hazelcast.jcache.provider.type", "server");
        
        Config cfg = new XmlConfigBuilder().build();
        instance = Hazelcast.newHazelcastInstance(cfg);
        return HazelcastServerCachingProvider.createCachingProvider(instance);
    }

}
