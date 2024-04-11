package org.ff4j.cache;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2024 FF4J
 * %%
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */

import java.io.Serializable;
import java.util.Map;

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

    /** Cache proxy. */
    private FF4jCacheProxy ff4JCacheProxy;
    
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

    /**
     * Parameterized constructor.
     *
     * @param fcp
     *      cache proxy
     */
    public Store2CachePollingWorker(FF4jCacheProxy fcp) {
        this(fcp.getTargetFeatureStore(),fcp.getTargetPropertyStore(),null);
        this.ff4JCacheProxy      = fcp;
    }

    
    /** {@inheritDoc} */
    @Override
    public void run() {

        try {
            FF4JCacheManager cacheManager;
            if (ff4JCacheProxy!=null) {
                cacheManager = new InMemoryCacheManager();
            } else {
                cacheManager = this.cacheManager;
            }

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

            if (ff4JCacheProxy!=null) {
                ff4JCacheProxy.setCacheManager(cacheManager);
            }
            
        } catch (Exception ex) {
            // Work in background (worker) failed 'silently'
            ex.printStackTrace();
        }
    }

}
