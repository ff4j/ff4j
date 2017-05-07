package org.ff4j.consul.store;

import org.ff4j.consul.ConsulConnection;
import org.ff4j.core.FeatureStore;
import org.ff4j.store.kv.KeyValueFeatureStore;
import org.ff4j.utils.mapping.JsonStringFeatureMapper;

/**
 * Generic {@link FeatureStore} to persist properties in a JCache (JSR107) compliant storage.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreConsul extends KeyValueFeatureStore < String > {
    
    /**
     * Default contructor.
     */
    public FeatureStoreConsul() {
        super();
    }
    
    /**
     * Initialization with cache manager.
     *
     * @param cacheManager
     */
    public FeatureStoreConsul(ConsulConnection connection) {
        super(connection, new JsonStringFeatureMapper());
    }
    
    
}
