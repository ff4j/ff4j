package org.ff4j.ignite.store;

import org.apache.ignite.Ignite;
import org.apache.ignite.configuration.IgniteConfiguration;
import org.ff4j.cache.FF4JCacheManager;
import org.ff4j.core.FeatureStore;
import org.ff4j.ignite.FF4jCacheManagerIgnite;
import org.ff4j.store.FeatureStoreJCache;
import org.ff4j.store.PropertyStoreJCache;


/**
 * Implementation of {@link FeatureStore} for ignite.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyStoreIgnite extends PropertyStoreJCache {

    /**
     * Default constructor.
     */
    public PropertyStoreIgnite() {
        this(new FF4jCacheManagerIgnite());
    }
    
    /**
     * Default constructor.
     */
    public PropertyStoreIgnite(String xmlConfigFileName) {
        this(new FF4jCacheManagerIgnite(xmlConfigFileName));
    }
            
    /**
     * Leverage on JCACHE but initialize from ignite.
     *
     * @param cacheManager
     */
    public PropertyStoreIgnite(IgniteConfiguration igniteConfig) {
        this(new FF4jCacheManagerIgnite(igniteConfig));
    }
    
    /**
     * Leverage on JCACHE but initialize from ignite.
     *
     * @param cacheManager
     */
    public PropertyStoreIgnite(Ignite ignite) {
        this(new FF4jCacheManagerIgnite(ignite));
    }
    
    /**
     * Init from ignite, cast manager (logic in {@link FeatureStoreJCache}).
     * 
     * @param cacheManager
     *      implementation of {@link FF4JCacheManager} for hazel cast
     */
    private PropertyStoreIgnite(FF4jCacheManagerIgnite cacheManager) {
        super(cacheManager);
    }

}
