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

import java.util.stream.Stream;

import org.ff4j.feature.Feature;
import org.ff4j.feature.repository.FeaturesRepository;

/**
 * Access to {@link FeaturesRepository} could generate some overhead and decrease performances. This is the reason why cache is provided
 * though proxies.
 * 
 * As applications are distributed, the cache itself could be distributed. The default implement is
 * {@link InMemoryFeatureStoreCacheProxy} but other are provided to use distributed cache system as redis or memcached.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class CacheProxyFeatures extends CacheProxy< String, Feature> implements FeaturesRepository {

    /**
     * Initialization through constructor.
     * 
     * @param store
     *            target store to retrieve features
     * @param cache
     *            cache manager to limit overhead of store
     */
    public CacheProxyFeatures(FeaturesRepository fStore, CacheManager< String, Feature > cache) {
        this.cacheManager = cache;
        this.targetStore  = fStore;
        this.scheduler    = new CachePollingSchedulerFeatures(fStore, cache);
    }
    
    /** {@inheritDoc} */
    @Override
    public void saveFeature(Feature feature) {
        getTargetFeatureStore().saveFeature(feature);
        cacheManager.put(feature.getUid(), feature);
    }

    /** {@inheritDoc} */
    @Override
    public void deleteFeature(String uid) {
        getTargetFeatureStore().deleteFeature(uid);
        cacheManager.evict(uid);
    }

    /** {@inheritDoc} */
    @Override
    public void toggleOn(String featureId) {
        // Reach target
        getTargetFeatureStore().toggleOn(featureId);
        // Modification => flush cache
        cacheManager.evict(featureId);
    }

    /** {@inheritDoc} */
    @Override
    public void toggleOff(String featureId) {
        // Reach target
        getTargetFeatureStore().toggleOff(featureId);
        // Cache Operations : As modification, flush cache for this
        cacheManager.evict(featureId);
    }   
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        // Create table for features but not only
        getTargetFeatureStore().createSchema();
    }
    
    /** {@inheritDoc} */
    @Override
    public Stream <String> listGroupNames() {
        // Cannot be sure of whole cache - do not test any feature one-by-one : accessing FeatureStore
        return getTargetFeatureStore().listGroupNames();
    }
    
    /** {@inheritDoc} */
    @Override
    public void toggleOnGroup(String groupName) {
        getTargetFeatureStore().toggleOnGroup(groupName);
        // Cannot know wich feature to work with (exceptional event) : flush cache
        cacheManager.clear();
    }

    /** {@inheritDoc} */
    @Override
    public void toggleOffGroup(String groupName) {
        getTargetFeatureStore().toggleOffGroup(groupName);
        // Cannot know wich feature to work with (exceptional event) : flush cache
        cacheManager.clear();
    }

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        // Cache cannot help you
        return getTargetFeatureStore().existGroup(groupName);
    }

    /** {@inheritDoc} */
    @Override
    public Stream <Feature> readGroup(String groupName) {
        // Cache cannot help you
        return getTargetFeatureStore().readGroup(groupName);
    }

    /** {@inheritDoc} */
    @Override
    public void addToGroup(String featureId, String groupName) {
        getTargetFeatureStore().addToGroup(featureId, groupName);
        cacheManager.evict(featureId);
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String featureId, String groupName) {
        getTargetFeatureStore().removeFromGroup(featureId, groupName);
        cacheManager.evict(featureId);
    }
    
    /**
     * Getter accessor for attribute 'target'.
     * 
     * @return current value of 'target'
     */
    public FeaturesRepository getTargetFeatureStore() {
        if (targetStore == null) {
            throw new IllegalArgumentException("ff4j-core: Target for cache proxy has not been provided");
        }
        return (FeaturesRepository) targetStore;
    }    

}
