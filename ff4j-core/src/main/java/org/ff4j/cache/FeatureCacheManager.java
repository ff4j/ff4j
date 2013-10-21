package org.ff4j.cache;

/*
 * #%L
 * FeatureCacheManager.java (ff4j-core) by Cedrick LUNVEN
 * %%
 * Copyright (C) 2013 Ff4J
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

import org.ff4j.core.Feature;

/**
 * Cache Layer on top of {@link FeatureStore} to enhance performances.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public interface FeatureCacheManager {

    /**
     * Remove everything present within cache.
     */
    void clear();

    /**
     * Remove a feature from cache by its identifier. Could be invoked for any modification of target feature through store or
     * when time-to-live reached.
     * 
     * @param featureId
     *            feature identifier
     */
    void evict(String featureId);

    /**
     * Add feature to cache.
     * 
     * @param feat
     *            target feature to be cached
     */
    void put(Feature feat);

    /**
     * Return feature stored in cache.
     * 
     * @param featureId
     *            target feature identifier
     * @return target feature if exist (could raise {@link FeatureNotFoundException} as FeatureStore).
     */
    Feature get(String featureId);

    /**
     * Access to embedded implementation of cache.
     * 
     * @return native implementation of cache.
     */
    Object getNativeCache();

}
