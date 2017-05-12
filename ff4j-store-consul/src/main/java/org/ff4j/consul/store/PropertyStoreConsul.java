package org.ff4j.consul.store;

/*
 * #%L
 * ff4j-store-consul
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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
