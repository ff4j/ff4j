package org.ff4j.v1.cache;

import java.io.Serializable;
import java.util.Map;

import org.ff4j.v1.core.Feature;
import org.ff4j.v1.core.FeatureStore;
import org.ff4j.v1.property.Property;
import org.ff4j.v1.property.store.PropertyStore;

/**
 * Working thread to poll and fetch data from store and copy to local cache.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class Store2CachePollingWorker implements Runnable, Serializable {

    /** Serial. */
    private static final long serialVersionUID = 8252550757489651166L;

    /** feature store. */
    private FeatureStore sourceFeatureStore;
    
    /** Property store. */
    private PropertyStore sourcePropertyStore;
    
    /** Target feature store to be proxified to cache features. */
    private FF4JCacheManager cacheManager;
    
    /**
     * Parameterized constructor.
     *
     * @param sf
     *      source feature store
     * @param sp
     *      source property store
     * @param cp
     *      target cache
     */
    public Store2CachePollingWorker(FeatureStore sf, PropertyStore sp, FF4JCacheManager cp) {
        this.sourceFeatureStore  = sf;
        this.sourcePropertyStore = sp;
        this.cacheManager        = cp;
    }
    
    /** {@inheritDoc} */
    @Override
    public void run() {
        try {
            
            if (sourceFeatureStore != null) {
                // Access the store, if failed an error is raised and cache is not cleared.
                Map < String, Feature > mapOfFeatures = sourceFeatureStore.readAll();
                // Clear cache
                cacheManager.clearFeatures();
                // Fill Cache
                for (Feature f : mapOfFeatures.values()) {
                    cacheManager.putFeature(f);
                }
            }
            
            if (sourcePropertyStore != null) {
                // Access the store, if failed an error is raised and cache is not cleared.
                Map < String, Property<?> > mapOfProperties = sourcePropertyStore.readAllProperties();
                // Clear cache
                cacheManager.clearProperties();
                // Fill Cache
                for (Property<?> p : mapOfProperties.values()) {
                    cacheManager.putProperty(p);
                }
            }
            
        } catch (Exception ex) {
            // Work in background (worker) failed 'silently'
            ex.printStackTrace();
        }
    }

}
