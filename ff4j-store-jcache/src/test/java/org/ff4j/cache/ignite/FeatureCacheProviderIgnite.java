package org.ff4j.cache.ignite;

import javax.cache.Cache;
import javax.cache.CacheManager;
import javax.cache.spi.CachingProvider;

import org.apache.ignite.Ignite;
import org.apache.ignite.Ignition;
import org.apache.ignite.cache.CacheAtomicityMode;
import org.apache.ignite.configuration.CacheConfiguration;
import org.ff4j.cache.FF4jJCacheManager;
import org.ff4j.core.Feature;
import org.ff4j.property.AbstractProperty;

/**
 * Proposition of specialization of {@link FF4jJCacheManager} for Apache Ignite.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class FeatureCacheProviderIgnite extends FF4jJCacheManager {
    
    /** Internal HazelCast Settings. */
    private Ignite ignite = null;
    
    /** {@inheritDoc} */
    @Override
    public CachingProvider initCachingProvider(String className) {
        ignite = Ignition.ignite(); 
        return null;
    }
    
    /** {@inheritDoc} */
    @Override
    protected CacheManager initCacheManager() {
        return null;
    }
    
    /** {@inheritDoc} */
    @Override
    protected Cache<String, Feature> createCacheForFeatures() {
        CacheConfiguration<String, Feature> cfg = new CacheConfiguration<String, Feature>();
        cfg.setName(CACHENAME_FEATURES);
        cfg.setAtomicityMode(CacheAtomicityMode.TRANSACTIONAL);
        return ignite.getOrCreateCache(cfg);
    }
    
    /** {@inheritDoc} */
    @Override
    @SuppressWarnings("rawtypes")
    protected Cache<String, AbstractProperty> createCacheForProperties() {
        CacheConfiguration<String, AbstractProperty> cfg = new CacheConfiguration<String, AbstractProperty>();
        cfg.setName(CACHENAME_PROPERTIES);
        cfg.setAtomicityMode(CacheAtomicityMode.TRANSACTIONAL);
        return ignite.getOrCreateCache(cfg);
    }

}
