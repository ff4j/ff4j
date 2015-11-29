package org.ff4j.cache;

import java.util.HashSet;
import java.util.Set;

import javax.cache.Cache;

import org.ff4j.core.Feature;
import org.ff4j.property.AbstractProperty;

/**
 * Implementation of {@link FF4JCacheManager} with reference interface JCache {@link Cache}.
 * 
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FF4jJCacheManager extends FF4jJCacheProvider implements FF4JCacheManager {    
    
    /** cache name of the features. */
    protected static final String CACHENAME_FEATURES      = "ff4jFeatures";
    
    /** cache name of the properties. */
    protected static final String CACHENAME_PROPERTIES    = "ff4jProperties";
    
    /** Implementing a JCache CacheProvider. */
    protected Cache<String, Feature> featuresCache;
    
    /** Implementing a JCache CacheProvider. */
    @SuppressWarnings("rawtypes")
    protected Cache<String, AbstractProperty> propertiesCache; 
    
    /**
     * Initialisation of internal caches.
     */
    public FF4jJCacheManager() {
        // Initialization of both caching provider and manager
        super();
        featuresCache   = createCacheForFeatures();
        propertiesCache = createCacheForProperties();
    }
    
    /**
     * Initialisation of internal caches.
     */
    public FF4jJCacheManager(String cachingProviderClassName) {
        // Initialization of both caching provider and manager
        super(cachingProviderClassName);
        featuresCache   = createCacheForFeatures();
        propertiesCache = createCacheForProperties();
    }
    
    
    /**
     * Default initialisation of cache.
     *
     * @return
     */
    protected Cache<String, Feature> createCacheForFeatures() {
        if (null == getCacheManager()) {
            throw new IllegalArgumentException("Cannot initialize Cache without the Cache manager");
        }
        if (null == getCacheManager().getCache(CACHENAME_FEATURES, String.class, Feature.class)) {
            getCacheManager().createCache(CACHENAME_FEATURES, getFeatureCacheConfiguration());
        }
        return getCacheManager().getCache(CACHENAME_FEATURES, String.class, Feature.class);
    }
    
    /**
     * Default initialisation of cache.
     *
     * @return
     */
    @SuppressWarnings("rawtypes")
    protected Cache<String, AbstractProperty> createCacheForProperties() {
        if (null == getCacheManager()) {
            throw new IllegalArgumentException("Cannot initialize Cache without the Cache manager");
        }
        if (null == getCacheManager().getCache(CACHENAME_PROPERTIES, String.class, AbstractProperty.class)) {
            getCacheManager().createCache(CACHENAME_PROPERTIES, getPropertyCacheConfiguration());
        }
        return getCacheManager().getCache(CACHENAME_PROPERTIES, String.class, AbstractProperty.class);
    }


    /** {@inheritDoc} */
    public Set<String> listCachedFeatureNames() {
        Set<String> keys = new HashSet<>();
        // Implements iterate, more elegant as stream how ?
        getFeaturesCache().forEach(e->keys.add(e.getKey()));
        return keys;
    }
    
    /** {@inheritDoc} */
    public Object getNativeCache() {
        if (featuresCache == null) {
            throw new IllegalStateException("Cache has not been initialized, please check");
        }
        return featuresCache;
    }

    /** {@inheritDoc} */
    public String getCacheProviderName() {
        if (featuresCache == null) return null;
        return "jCache:" + featuresCache.getName() + 
                ":" + featuresCache.getCacheManager().getCachingProvider().toString();
    }   

    /** {@inheritDoc} */
    @Override
    public void clearFeatures() {
        getFeaturesCache().clear();
    }

    /** {@inheritDoc} */
    @Override
    public void clearProperties() {
        getPropertiesCache().clear();
    }

    /** {@inheritDoc} */
    @Override
    public void evictFeature(String featureId) {
        if (getFeaturesCache().containsKey(featureId)) {
            getFeaturesCache().remove(featureId);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void evictProperty(String propertyName) {
        if (getPropertiesCache().containsKey(propertyName)) {
            getPropertiesCache().remove(propertyName);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void putFeature(Feature feat) {
        getFeaturesCache().put(feat.getUid(), feat);
    }

    /** {@inheritDoc} */
    @Override
    public void putProperty(AbstractProperty<?> feat) {
        getPropertiesCache().put(feat.getName(), feat);
    }

    /** {@inheritDoc} */
    @Override
    public Feature getFeature(String featureId) {
        return getFeaturesCache().get(featureId);
    }

    /** {@inheritDoc} */
    @Override
    public AbstractProperty<?> getProperty(String name) {
        return getPropertiesCache().get(name);
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> listCachedPropertyNames() {
        Set<String> keys = new HashSet<>();
        // Implements iterate, more elegant as stream how ?
        getPropertiesCache().forEach(e->keys.add(e.getKey()));
        return keys;
    }

    /** {@inheritDoc} */
    @Override
    public Object getFeatureNativeCache() {
        return getFeaturesCache();
    }

    /** {@inheritDoc} */
    @Override
    public Object getPropertyNativeCache() {
        return getPropertiesCache();
    }

    /**
     * Getter accessor for attribute 'featuresCache'.
     *
     * @return
     *       current value of 'featuresCache'
     */
    public Cache<String, Feature> getFeaturesCache() {
        return featuresCache;
    }

    /**
     * Setter accessor for attribute 'featuresCache'.
     * @param featuresCache
     * 		new value for 'featuresCache '
     */
    public void setFeaturesCache(Cache<String, Feature> featuresCache) {
        this.featuresCache = featuresCache;
    }

    /**
     * Getter accessor for attribute 'propertiesCache'.
     *
     * @return
     *       current value of 'propertiesCache'
     */
    @SuppressWarnings("rawtypes")
    public Cache<String, AbstractProperty> getPropertiesCache() {
        return propertiesCache;
    }

}
