package org.ff4j.cache;

import org.ff4j.feature.Feature;
import org.ff4j.property.Property;

import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

/**
 * Cache Layer on top of Backend. to enhance performances.
 */
public interface FF4jCacheRepository {

    // -------------------------------------
    // ---- Work with Features   -----------
    // -------------------------------------

    /**
     * Remove everything present within feature cache.
     *
     * @param workspace
     *      current workspace
     */
    void clearFeatures(String workspace);

    /**
     * Remove everything present within feature cache.
     *
     * @param workspace
     *      current workspace
     */
    void clearFeaturesFlags(String workspace);

    /**
     * Remove everything present within feature cache.
     *
     * @param workspace
     *      current workspace
     * @param feature
     *      current feature
     */
    void cacheFeature(String workspace, Feature feature);

    /**
     * Add feature to cache.
     *
     * @param workspace
     *      current workspace
     * @param uid
     *      target feature to be cached
     * @param flag
     *      value for feature
     */
    void cacheFeatureFlag(String workspace, String uid, boolean flag);

    /**
     * Remove a feature from cache by its identifier. Could be invoked for any modification of target feature through store or
     * when time-to-live reached.
     *
     * @param workspace
     *      current workspace
     * @param uid
     *      feature identifier
     */
    void evictFeatureFlag(String workspace, String uid);

    /**
     * Remove a feature from cache by its identifier. Could be invoked for any modification of target feature through store or
     * when time-to-live reached.
     *
     * @param workspace
     *      current workspace
     * @param uid
     *      feature identifier
     */
    void evictFeature(String workspace, String uid);

    /**
     * Retrieve a feature.
     *
     * @param workspace
     *      current workspace
     * @param uid
     *      current feature id
     * @return
     */
    Optional<Feature> findFeature(String workspace, String uid);

    /**
     * Retrieve a feature flag
     *
     * @param workspace
     *      current workspace
     * @param uid
     *      current feature id
     * @return
     */
    Optional<Boolean> findFeatureFlag(String workspace, String uid);

    /**
     * List all features
     *
     * @param workspace
     *      current workspace
     * @return
     *      cached features
     */
    Stream<Feature> getFeatures(String workspace);

    /**
     * List all features
     *
     * @param workspace
     *      current workspace
     * @return
     *      cached features
     */
    Map<String, Boolean> getFeaturesFlags(String workspace);

    // -------------------------------------
    // ---- Work with Properties -----------
    // -------------------------------------

    /**
     * Remove everything present within properties cache.
     *
     * @param wokspace
     *      current wokspace
     */
    void clearProperties(String wokspace);

    /**
     * Add property to cache.
     *
     * @param workspace
     *      current workspace
     * @param feat
     *      target property to be cached
     */
    void cacheProperty(String workspace, Property<?> feat);

    /**
     * Remove a property from cache by its identifier. Could be invoked for any modification of target feature through store or
     * when time-to-live reached.
     *
     * @param workspace
     *      current workspace
     * @param propertyName
     *      property unique identifier
     */
    void evictProperty(String workspace, String propertyName);

    /**
     * Retrieve a property.
     *
     * @param workspace
     *      current workspace
     * @param uid
     *      current feature id
     * @return
     */
    Optional<Property<?>> findProperty(String workspace, String uid);

    // -----------------------------------------
    // ---- Work with Cache Metadata -----------
    // -----------------------------------------

    /**
     * Access to embedded implementation of cache for Features.
     * 
     * @return native implementation of cache.
     */
    Object getCacheFeaturesFlags();
    
    /**
     * Access to embedded implementation of cache for Properties
     * 
     * @return native implementation of cache.
     */
    Object getCacheProperties();

    /**
     * Get name of expected cache.
     * 
     * @return target cache name
     */
    String getCacheProviderName();
    
    /**
     * Customize behaviour when an error is thrown.
     */
    default void onException(Throwable error) {};

}
