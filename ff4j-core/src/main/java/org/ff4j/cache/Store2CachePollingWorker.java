package org.ff4j.cache;

import java.io.Serializable;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.property.Property;
import org.ff4j.property.store.PropertyStore;

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
        cacheManager.clearFeatures();
        for (Feature f : sourceFeatureStore.readAll().values()) {
            cacheManager.putFeature(f);
        }
        cacheManager.clearProperties();
        for (Property<?> p : sourcePropertyStore.readAllProperties().values()) {
            cacheManager.putProperty(p);
        }
    }

}
