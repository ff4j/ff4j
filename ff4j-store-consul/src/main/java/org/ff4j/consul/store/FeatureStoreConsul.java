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
