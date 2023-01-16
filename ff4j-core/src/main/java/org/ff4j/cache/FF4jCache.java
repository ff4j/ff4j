package org.ff4j.cache;

import org.ff4j.feature.Flag;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyString;

import java.util.Optional;
import java.util.Set;

/**
 * Cache Layer on top of Backend. to enhance performances.
 */
public interface FF4jCache {

    // -------------------------------------
    // ---- Work with Features   -----------
    // -------------------------------------

    /**
     * List feature names in cache.
     *
     * @param namespace
     *      current namespace
     * @return
     *      feature names in cache
     */
    Set < String > findAllFeatureNames(String namespace);

    /**
     * Remove everything present within feature cache.
     *
     * @param namespace
     *      current namespace
     */
    void clearFeatures(String namespace);

    /**
     * Return {@link Flag} stored in cache.
     *
     * @param namespace
     *      current namespace
     * @param featureId
     *            target feature identifier
     * @return target feature if exists
     */
    Optional<Flag> findFeatureById(String namespace, String featureId);

    /**
     * Add feature to cache.
     *
     * @param namespace
     *      current namespace
     * @param feat
     *      target feature to be cached
     */
    void putFeature(String namespace, Flag feat);

    /**
     * Remove a feature from cache by its identifier. Could be invoked for any modification of target feature through store or
     * when time-to-live reached.
     *
     * @param namespace
     *      current namespace
     * @param featureId
     *      feature identifier
     */
    void evictFeature(String namespace, String featureId);

    // -------------------------------------
    // ---- Work with Properties -----------
    // -------------------------------------

    /**
     * Remove everything present within properties cache.
     *
     * @param namespace
     *      current namespace
     */
    void clearProperties(String namespace);

    /**
     * Add property to cache.
     *
     * @param namespace
     *      current namespace
     * @param feat
     *      target property to be cached
     */
    void putProperty(String namespace, Property<?> feat);

    /**
     * Remove a property from cache by its identifier. Could be invoked for any modification of target feature through store or
     * when time-to-live reached.
     *
     * @param namespace
     *      current namespace
     * @param propertyName
     *      property unique identifier
     */
    void evictProperty(String namespace, String propertyName);

    /**
     * Return {@link PropertyString} stored in cache.
     *
     * @param namespace
     *      current namespace
     * @param featureId
     *            target feature identifier
     * @return target property if exists
     */
    Optional<Property<?>> findPropertyById(String namespace, String featureId);

    /**
     * List property names in cache.
     *
     * @param namespace
     *      current namespace
     * @return
     *      feature names in cache
     */
    Set < String > findAllPropertiesNames(String namespace);

    // -----------------------------------------
    // ---- Work with Cache Metadata -----------
    // -----------------------------------------

    /**
     * Access to embedded implementation of cache for Features.
     * 
     * @return native implementation of cache.
     */
    Object getFeatureNativeCache();
    
    /**
     * Access to embedded implementation of cach for Properties
     * 
     * @return native implementation of cache.
     */
    Object getPropertyNativeCache();

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
