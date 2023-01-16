package org.ff4j.cache;

import org.ff4j.feature.Flag;
import org.ff4j.namespace.exception.NamespaceNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.utils.Assert;

import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.*;

/**
 * Custom implementation of local cache with WeakMaps and {@link FF4jCacheEntry}.
 */
public class FF4jCacheInMemory implements FF4jCache {

    /** Cached Feature Map */
    private final Map<String, Map<String, FF4jCacheEntry<Flag>>> featuresCache = new WeakHashMap<>();
    
    /** Cached Property Map */
    private final Map<String, Map<String, FF4jCacheEntry<Property<?>>>> propertyCache= new WeakHashMap<>();

    /** Time to live for entries. */
    private final Duration timeToLive;

    /**
     * Default constructor
     */
    public FF4jCacheInMemory() {
        this(Duration.of(60, ChronoUnit.SECONDS));
    }

    /**
     * Constructor with a TTL.
     *
     * @param timeToLive
     *      time to live
     */
    public FF4jCacheInMemory(Duration timeToLive) {
        this.timeToLive = timeToLive;
    }

    // -----------------------------------------
    // ---- Work with Cache Metadata -----------
    // -----------------------------------------

    /** {@inheritDoc} */
    @Override
    public String getCacheProviderName() { return "InMemory";}

    /** {@inheritDoc} */
    @Override
    public Object getFeatureNativeCache() { return featuresCache; }

    /** {@inheritDoc} */
    @Override
    public Object getPropertyNativeCache() {
        return propertyCache;
    }

    @Override
    public void onException(Throwable error) {
        FF4jCache.super.onException(error);
    }

    // -----------------------------------------
    // ---- Work with Features       -----------
    // -----------------------------------------

    /** {@inheritDoc} */
    @Override
    public Set<String> findAllFeatureNames(String namespace) {
        assertNamespaceExists(namespace);
        return featuresCache.get(namespace).keySet();
    }

    /** {@inheritDoc} */
    @Override
    public void clearFeatures(String namespace) {
        assertNamespaceExists(namespace);
        featuresCache.get(namespace).clear();
    }

    /** {@inheritDoc} */
    @Override
    public Optional<Flag> findFeatureById(String namespace, String featureId) {
        assertNamespaceExists(namespace);
        FF4jCacheEntry<Flag> cacheEntry = featuresCache.get(namespace).get(featureId);
        if (cacheEntry != null) {
            if (!cacheEntry.isOutdated()) {
                return Optional.ofNullable(cacheEntry.getEntry());
            } else {
                evictFeature(namespace, featureId);
            }
        }
        return Optional.empty();
    }

    /** {@inheritDoc} */
    @Override
    public void putFeature(String namespace, Flag feat) {
        Assert.assertHasLength(namespace);
        Assert.assertNotNull(feat);
        Assert.assertHasLength(feat.getUid());
        if (!featuresCache.containsKey(namespace)) featuresCache.put(namespace, new HashMap<>());
        featuresCache.get(namespace).put(feat.getUid(), new FF4jCacheEntry<>(feat, timeToLive));
    }

    /** {@inheritDoc} */
    @Override
    public void evictFeature(String namespace, String featureId) {
        assertNamespaceExists(namespace);
        Assert.assertHasLength(featureId);
        featuresCache.get(namespace).remove(featureId);
    }

    // -----------------------------------------
    // ---- Work with Properties     -----------
    // -----------------------------------------

    /** {@inheritDoc} */
    @Override
    public void clearProperties(String namespace) {
        assertNamespaceExists(namespace);
        propertyCache.get(namespace).clear();
    }

    /** {@inheritDoc} */
    @Override
    public void putProperty(String namespace, Property<?> property) {
        Assert.assertHasLength(namespace);
        Assert.assertNotNull(property);
        Assert.assertHasLength(property.getUid());
        if (!propertyCache.containsKey(namespace)) propertyCache.put(namespace, new HashMap<>());
        propertyCache.get(namespace).put(property.getUid(), new FF4jCacheEntry<>(property, timeToLive));
    }

    /** {@inheritDoc} */
    @Override
    public void evictProperty(String namespace, String propertyUid) {
        assertNamespaceExists(namespace);
        Assert.assertHasLength(propertyUid);
        propertyCache.get(namespace).remove(propertyUid);
    }

    /** {@inheritDoc} */
    @Override
    public Optional<Property<?>> findPropertyById(String namespace, String propertyUid) {
        assertNamespaceExists(namespace);
        FF4jCacheEntry<Property<?>> cacheEntry = propertyCache.get(namespace).get(propertyUid);
        if (cacheEntry != null) {
            if (!cacheEntry.isOutdated()) {
                return Optional.ofNullable(cacheEntry.getEntry());
            } else {
                evictProperty(namespace, propertyUid);
            }
        }
        return Optional.empty();
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> findAllPropertiesNames(String namespace) {
        assertNamespaceExists(namespace);
        return propertyCache.get(namespace).keySet();
    }

    /**
     * Test namespace existence.
     *
     * @param namespace
     *      namespace identifier
     */
    private void assertNamespaceExists(String namespace) {
        Assert.assertHasLength(namespace);
        if (featuresCache.containsKey(namespace)) {
            throw new NamespaceNotFoundException(namespace);
        }
    }

}