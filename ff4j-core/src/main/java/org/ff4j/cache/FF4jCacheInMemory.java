package org.ff4j.cache;

import org.ff4j.feature.Feature;
import org.ff4j.workspace.exception.WorkspaceNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.utils.Assert;

import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.stream.Stream;

/**
 * Custom implementation of local cache with WeakMaps and {@link FF4jCacheEntry}.
 */
public class FF4jCacheInMemory implements FF4jCacheRepository {

    /**
     * Cache for feature flags : workspace -> uid -> flag
     */
    private final Map<String, Map<String, FF4jCacheEntry<Boolean>>> cacheFeatureFlags = new WeakHashMap<>();

    /**
     * Cache for feature flags : workspace -> uid -> flag
     */
    private final Map<String, Map<String, FF4jCacheEntry<Feature>>> cacheFeatures = new WeakHashMap<>();

    /**
     * Cache for properties : workspace -> uid -> property
     */
    private final Map<String, Map<String, FF4jCacheEntry<Property<?>>>> cacheProperties = new WeakHashMap<>();

    /**
     * Time to live for entries.
     */
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
     *         time to live
     */
    public FF4jCacheInMemory(Duration timeToLive) {
        this.timeToLive = timeToLive;
    }

    // -----------------------------------------
    // ---- Work with Cache Metadata -----------
    // -----------------------------------------

    /**
     * {@inheritDoc}
     */
    @Override
    public String getCacheProviderName() {
        return "InMemory";
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object getCacheFeaturesFlags() {
        return cacheFeatureFlags;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Object getCacheProperties() {
        return cacheProperties;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void onException(Throwable error) {
        FF4jCacheRepository.super.onException(error);
    }

    // -----------------------------------------
    // ---- Work with Features       -----------
    // -----------------------------------------

    /**
     * {@inheritDoc}
     */
    @Override
    public void clearFeatures(String workspace) {
        cacheFeatures.put(workspace, new HashMap<>());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void clearFeaturesFlags(String workspace) {
        cacheFeatureFlags.put(workspace, new HashMap<>());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void cacheFeature(String workspace, Feature feature) {
        Assert.assertHasLength(workspace);
        Assert.assertNotNull(feature);
        if (!cacheFeatures.containsKey(workspace)) cacheFeatures.put(workspace, new HashMap<>());
        cacheFeatures.get(workspace).put(feature.getUid(), new FF4jCacheEntry<>(feature));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void cacheFeatureFlag(String workspace, String uid, boolean flag) {
        Assert.assertHasLength(workspace);
        Assert.assertHasLength(uid);
        if (!cacheFeatureFlags.containsKey(workspace)) cacheFeatureFlags.put(workspace, new HashMap<>());
        cacheFeatureFlags.get(workspace).put(uid, new FF4jCacheEntry<>(flag));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Stream<Feature> getFeatures(String workspace) {
        Assert.assertHasLength(workspace);
        return cacheFeatures.containsKey(workspace) ?
                cacheFeatures.get(workspace).values().stream()
                        .map(FF4jCacheEntry::getEntry) : Stream.empty();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Optional<Feature> findFeature(String workspace, String uid) {
        if (cacheFeatures.containsKey(workspace)) {
            FF4jCacheEntry<Feature> cacheEntry = cacheFeatures.get(workspace).get(uid);
            if (cacheEntry != null) {
                if (!cacheEntry.isOutdated()) {
                    return Optional.ofNullable(cacheEntry.getEntry());
                } else {
                    evictProperty(workspace, uid);
                }
            }
        }
        return Optional.empty();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Optional<Boolean> findFeatureFlag(String workspace, String uid) {
        if (cacheFeatureFlags.containsKey(workspace)) {
            FF4jCacheEntry<Boolean> cacheEntry = cacheFeatureFlags.get(workspace).get(uid);
            if (cacheEntry != null) {
                if (!cacheEntry.isOutdated()) {
                    return Optional.ofNullable(cacheEntry.getEntry());
                } else {
                    evictProperty(workspace, uid);
                }
            }
        }
        return Optional.empty();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Boolean> getFeaturesFlags(String workspace) {
        Assert.assertHasLength(workspace);
        Map<String, Boolean> returnedMap = new HashMap<>();
        if (cacheFeatureFlags.containsKey(workspace)) {
            cacheFeatureFlags
                    .get(workspace)
                    .forEach((k,v) -> returnedMap.put(k, v.getEntry()));
        }
        return returnedMap;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void evictFeatureFlag(String workspace, String featureId) {
        assertNamespaceExists(workspace);
        Assert.assertHasLength(featureId);
        cacheFeatureFlags.get(workspace).remove(featureId);
    }

    @Override
    public void evictFeature(String workspace, String uid) {

    }

    // -----------------------------------------
    // ---- Work with Properties     -----------
    // -----------------------------------------

    /**
     * {@inheritDoc}
     */
    @Override
    public Optional<Property<?>> findProperty(String workspace, String propertyUid) {
        if (cacheProperties.containsKey(workspace)) {
            FF4jCacheEntry<Property<?>> cacheEntry = cacheProperties.get(workspace).get(propertyUid);
            if (cacheEntry != null) {
                if (!cacheEntry.isOutdated()) {
                    return Optional.ofNullable(cacheEntry.getEntry());
                } else {
                    evictProperty(workspace, propertyUid);
                }
            }
        }
        return Optional.empty();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void clearProperties(String workspace) {
        assertNamespaceExists(workspace);
        cacheProperties.get(workspace).clear();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void cacheProperty(String workspace, Property<?> property) {
        Assert.assertHasLength(workspace);
        Assert.assertNotNull(property);
        Assert.assertHasLength(property.getUid());
        if (!cacheProperties.containsKey(workspace)) cacheProperties.put(workspace, new HashMap<>());
        cacheProperties.get(workspace).put(property.getUid(), new FF4jCacheEntry<>(property, timeToLive));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void evictProperty(String workspace, String propertyUid) {
        assertNamespaceExists(workspace);
        Assert.assertHasLength(propertyUid);
        cacheProperties.get(workspace).remove(propertyUid);
    }

    /**
     * Test namespace existence.
     *
     * @param namespace
     *         namespace identifier
     */
    private void assertNamespaceExists(String namespace) {
        Assert.assertHasLength(namespace);
        if (cacheFeatureFlags.containsKey(namespace)) {
            throw new WorkspaceNotFoundException(namespace);
        }
    }
}