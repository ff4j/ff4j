package org.ff4j.consul.store;

import org.ff4j.consul.ConsulConnection;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.store.kv.KeyValuePropertyStore;
import org.ff4j.utils.mapping.JsonStringPropertyMapper;

/**
 * Generic {@link PropertyStore} to persist properties in a JCache (JSR107) compliant storage.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class PropertyStoreConsul extends KeyValuePropertyStore < String > {
    
    /**
     * Default contructor.
     */
    public PropertyStoreConsul() {
        super();
    }
    
    /**
     * Initialization with cache manager.
     *
     * @param cacheManager
     */
    public PropertyStoreConsul(ConsulConnection connection) {
        super(connection, new JsonStringPropertyMapper());
    }

}
