package org.ff4j.cache;

import org.ff4j.FF4jClient;
import org.ff4j.backend.Backend;
import org.ff4j.feature.Flag;
import org.ff4j.property.Property;

import java.util.stream.Stream;

/**
 * Working thread to poll and fetch data from store and copy to local cache.
 */
public class FF4jCacheWorker implements Runnable {

    /** Reference to ff4j. */
    private FF4jClient ff4j;
    
    /** Target feature store to be proxified to cache features. */
    private final FF4jCache cache;

    /** Worker for a namespace. */
    private final String namespace;
    
    /**
     * Constructor.
     *
     * @param ff4j
     *      reference to ff4j
     * @param cacheManager
     *      cache manager
     */
    public FF4jCacheWorker(FF4jClient ff4j, FF4jCache cacheManager, String namespace) {
        this.ff4j       = ff4j;
        this.cache      = cacheManager;
        this.namespace  = namespace;
    }

    /** {@inheritDoc} */
    @Override
    public void run() {
        try {
            // Access store first, if failed an error is raised and cache is not cleared.
            Backend accessedBackend = ff4j.getBackend();
            Stream<Flag>     features   = accessedBackend.findAllFeatures(namespace);
            Stream<Property<?>> properties = accessedBackend.findAllProperties(namespace);
            // Clear cache
            cache.clearFeatures(namespace);
            cache.clearFeatures(namespace);
            // Populate cache
            features.forEach(f -> cache.putFeature(namespace, f));
            properties.forEach(p -> cache.putProperty(namespace, p));
        } catch (Exception ex) {
            // Work in background (worker) failed 'silently'
            ex.printStackTrace();
        }
    }

}
