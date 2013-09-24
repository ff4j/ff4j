package org.ff4j.cache;

import org.ff4j.core.Feature;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.store.FeatureStore;

/**
 * Cache Layer on top of {@link FeatureStore} to enhance performances.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public interface CacheProvider {

    /**
     * Remove everything present within cache.
     */
    void clear();

    /**
     * Remove a feature from cache by its identifier. Could be invoked for any modification of target feature through store or
     * when time-to-live reached.
     * 
     * @param featureId
     *            feature identifier
     */
    void evict(String featureId);

    /**
     * Add feature to cache.
     * 
     * @param feat
     *            target feature to be cached
     */
    void put(Feature feat);

    /**
     * Return feature stored in cache.
     * 
     * @param featureId
     *            target feature identifier
     * @return target feature if exist (could raise {@link FeatureNotFoundException} as FeatureStore).
     */
    Feature get(String featureId);

    /**
     * Access to embedded implementation of cache.
     * 
     * @return native implementation of cache.
     */
    Object getNativeCache();

}
