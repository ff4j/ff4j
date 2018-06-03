
package org.ff4j.cache;

/*-
 * #%L
 * ff4j-core
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

import java.io.Serializable;
import java.util.Optional;
import java.util.stream.Stream;

import org.ff4j.FF4jEntity;
import org.ff4j.feature.Feature;
import org.ff4j.feature.exception.FeatureNotFoundException;

/**
 * Proposition for caching of features and properties locally.
 *
 * @author Cedrick LUNVEN  (@clunven)
 *
 * @param <K>
 *      key (String most of the time)
 * @param <V>
 *      value (getUid())
 *      
 * @since 2.x
 */
public interface CacheManager < K extends Serializable , V extends FF4jEntity<?>> {
    
    /**
     * Get name of expected cache.
     * 
     * @return target cache name
     */
    String getCacheProviderName();
    
    /**
     * Access to embedded implementation of cache for Features.
     * 
     * @return native implementation of cache.
     */
    Object getNativeCache();
    
    /**
     * Remove everything present within feature cache.
     */
    void clear();

    /**
     * Remove a feature from cache by its identifier. Could be invoked for any modification of target feature through store or
     * when time-to-live reached.
     * 
     * @param featureId
     *            feature identifier
     */
    void evict(K key);

    /**
     * Add feature to cache.
     * 
     * @param feat
     *            target feature to be cached
     */
    void put(K key, V value);
    
    /**
     * Add feature to cache.
     * 
     * @param feat
     *            target feature to be cached
     */
    void put(V value);

    /**
     * Return {@link Feature} stored in cache.
     * 
     * @param featureId
     *            target feature identifier
     * @return target feature if exist (could raise {@link FeatureNotFoundException} as FeatureStore).
     */
    Optional<V> get(K featureId);
    
    /**
     * List feature names in cache.
     *
     * @return
     *      feature names in cache
     */
    Stream < String > keys();
   
}
