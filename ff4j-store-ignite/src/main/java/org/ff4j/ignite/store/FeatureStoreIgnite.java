package org.ff4j.ignite.store;

import org.apache.ignite.Ignite;
import org.apache.ignite.configuration.IgniteConfiguration;
import org.ff4j.cache.FF4JCacheManager;
import org.ff4j.core.FeatureStore;
import org.ff4j.ignite.FF4jCacheManagerIgnite;
import org.ff4j.store.FeatureStoreJCache;

/**
 * Implementation of {@link FeatureStore} for hazelcast.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class FeatureStoreIgnite extends FeatureStoreJCache {

    /**
     * Default constructor.
     */
    public FeatureStoreIgnite() {
        this(new FF4jCacheManagerIgnite());
    }
    
    /**
     * Default constructor.
     */
    public FeatureStoreIgnite(String xmlConfigFileName) {
        this(new FF4jCacheManagerIgnite(xmlConfigFileName));
    }
            
    /**
     * Leverage on JCACHE but initialize from Hazelcast.
     *
     * @param cacheManager
     */
    public FeatureStoreIgnite(IgniteConfiguration igniteConfig) {
        this(new FF4jCacheManagerIgnite(igniteConfig));
    }
    
    /**
     * Leverage on JCACHE but initialize from Hazelcast.
     *
     * @param cacheManager
     */
    public FeatureStoreIgnite(Ignite ignite) {
        this(new FF4jCacheManagerIgnite(ignite));
    }
    
    /**
     * Init from hazelcast, cast manager (logic in {@link FeatureStoreJCache}).
     * 
     * @param cacheManager
     *      implementation of {@link FF4JCacheManager} for hazel cast
     */
    private FeatureStoreIgnite(FF4jCacheManagerIgnite cacheManager) {
        super(cacheManager);
    }
}
