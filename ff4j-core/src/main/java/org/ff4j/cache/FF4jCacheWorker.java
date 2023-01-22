package org.ff4j.cache;

import org.ff4j.FF4j;
import org.ff4j.backend.Backend;
import org.ff4j.feature.Feature;
import org.ff4j.property.Property;

import java.util.Map;
import java.util.stream.Stream;

/**
 * Working thread to poll and fetch data from store and copy to local cache.
 */
public class FF4jCacheWorker implements Runnable {

    /** Reference to ff4j. */
    private final FF4j ff4j;
    
    /** Access to cache. */
    private final FF4jCacheRepository cache;

    /** Worker for a namespace. */
    private final String workspace;
    
    /**
     * Constructor.
     *
     * @param ff4j
     *      reference to ff4j
     * @param cacheManager
     *      cache manager
     */
    public FF4jCacheWorker(FF4j ff4j, FF4jCacheRepository cacheManager, String workspace) {
        this.ff4j       = ff4j;
        this.cache      = cacheManager;
        this.workspace = workspace;
    }

    /** {@inheritDoc} */
    @Override
    public void run() {
        try {
            // Access store first, if failed an error is raised and cache is not cleared.
            Backend accessedBackend = ff4j.getBackendOperations();
            Stream<Feature>     features   = accessedBackend.getFeatures(workspace);
            Stream<Property<?>> properties = accessedBackend.getProperties(workspace);
            Map<String, Boolean> flags = accessedBackend.getFeatureFlags(workspace);
            // Clear cache
            cache.clearFeatures(workspace);
            cache.clearFeatures(workspace);
            cache.clearFeaturesFlags(workspace);
            // Populate cache
            features.forEach(f -> cache.cacheFeature(workspace, f) );
            properties.forEach(p -> cache.cacheProperty(workspace, p));
            flags.forEach((k,v) ->cache.cacheFeatureFlag(workspace, k, v));
        } catch (Exception ex) {
            // Work in background (worker) failed 'silently'
            ex.printStackTrace();
        }
    }

}
