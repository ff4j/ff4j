package org.ff4j.cache;

/*
 * #%L
 * ff4j-core
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

import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyString;

/**
 * Cache Layer on top of {@link FeatureStore} to enhance performances.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public interface FF4JCacheManager {

    /**
     * Remove everything present within feature cache.
     */
    void clearFeatures();
    
    /**
     * Remove everything present within properties cache.
     */
    void clearProperties();

    /**
     * Remove a feature from cache by its identifier. Could be invoked for any modification of target feature through store or
     * when time-to-live reached.
     * 
     * @param featureId
     *            feature identifier
     */
    void evictFeature(String featureId);
    
    /**
     * Remove a property from cache by its identifier. Could be invoked for any modification of target feature through store or
     * when time-to-live reached.
     * 
     * @param propertyName
     *            property unique identifier
     */
    void evictProperty(String propertyName);

    /**
     * Add feature to cache.
     * 
     * @param feat
     *            target feature to be cached
     */
    void putFeature(Feature feat);
    
    /**
     * Add property to cache.
     * 
     * @param feat
     *            target property to be cached
     */
    void putProperty(Property<?> feat);

    /**
     * Return {@link Feature} stored in cache.
     * 
     * @param featureId
     *            target feature identifier
     * @return target feature if exist (could raise {@link FeatureNotFoundException} as FeatureStore).
     */
    Feature getFeature(String featureId);
    
    /**
     * Return {@link PropertyString} stored in cache.
     * 
     * @param featureId
     *            target feature identifier
     * @return target feature if exist (could raise {@link FeatureNotFoundException} as FeatureStore).
     */
    Property<?> getProperty(String featureId);
    
    /**
     * List feature names in cache.
     *
     * @return
     *      feature names in cache
     */
    Set < String > listCachedFeatureNames();
    
    /**
     * List property names in cache.
     *
     * @return
     *      feature names in cache
     */
    Set < String > listCachedPropertyNames();

    /**
     * Access to embedded implementation of cache for Features.
     * 
     * @return native implementation of cache.
     */
    Object getFeatureNativeCache();
    
    /**
     * Access to embedded implementation of cach for Properties
     * 
     * @return native implementation of cache.
     */
    Object getPropertyNativeCache();

    /**
     * Get name of expected cache.
     * 
     * @return target cache name
     */
    String getCacheProviderName();

}
