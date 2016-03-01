package org.ff4j.cache;

/*
 * #%L ff4j-core %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License"); you may not use
 * this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import java.util.Map;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.property.Property;
import org.ff4j.property.store.PropertyStore;

/**
 * Access to {@link FeatureStore} could generate some overhead and decrease performances. This is the reason why cache is provided
 * though proxies.
 * 
 * As applications are distributed, the cache itself could be distributed. The default implement is
 * {@link InMemoryFeatureStoreCacheProxy} but other are provided to use distributed cache system as redis or memcached.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class FF4jCacheProxy implements FeatureStore, PropertyStore {

    /** Target feature store to be proxified to cache features. */
    private FeatureStore targetFeatureStore;

    /** Target property store to be proxified to cache properties. */
    private PropertyStore targetPropertyStore;

    /** cache manager. */
    private FF4JCacheManager cacheManager;

    /**
     * Allow Ioc and defeine default constructor.
     */
    public FF4jCacheProxy() {}

    /**
     * Initialization through constructor.
     * 
     * @param store
     *            target store to retrieve features
     * @param cache
     *            cache manager to limit overhead of store
     */
    public FF4jCacheProxy(FeatureStore fStore, PropertyStore pStore, FF4JCacheManager cache) {
        this.cacheManager = cache;
        this.targetFeatureStore = fStore;
        this.targetPropertyStore = pStore;
    }

    /** {@inheritDoc} */
    @Override
    public void enable(String featureId) {
        // Reach target
        getTargetFeatureStore().enable(featureId);
        // Modification => flush cache
        getCacheManager().evictFeature(featureId);
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String featureId) {
        // Reach target
        getTargetFeatureStore().disable(featureId);
        // Cache Operations : As modification, flush cache for this
        getCacheManager().evictFeature(featureId);
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String featureId) {
        // not in cache but maybe created from last access
        if (getCacheManager().getFeature(featureId) == null) {
            return getTargetFeatureStore().exist(featureId);
        }
        return true;
    }

    /** {@inheritDoc} */
    @Override
    public void create(Feature fp) {
        getTargetFeatureStore().create(fp);
        getCacheManager().putFeature(fp);
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String featureUid) {
        Feature fp = getCacheManager().getFeature(featureUid);
        // not in cache but may has been created from now
        if (null == fp) {
            fp = getTargetFeatureStore().read(featureUid);
            getCacheManager().putFeature(fp);
        }
        return fp;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        // Cannot be sure of whole cache - do not test any feature one-by-one : accessing FeatureStore
        return getTargetFeatureStore().readAll();
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> readAllGroups() {
        // Cannot be sure of whole cache - do not test any feature one-by-one : accessing FeatureStore
        return getTargetFeatureStore().readAllGroups();
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String featureId) {
        // Access target store
        getTargetFeatureStore().delete(featureId);
        // even is not present, evict won't failed
        getCacheManager().evictFeature(featureId);
    }

    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        getTargetFeatureStore().update(fp);
        getCacheManager().evictFeature(fp.getUid());
    }

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String featureId, String roleName) {
        getTargetFeatureStore().grantRoleOnFeature(featureId, roleName);
        getCacheManager().evictFeature(featureId);
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String featureId, String roleName) {
        getTargetFeatureStore().removeRoleFromFeature(featureId, roleName);
        getCacheManager().evictFeature(featureId);
    }

    /** {@inheritDoc} */
    @Override
    public void enableGroup(String groupName) {
        getTargetFeatureStore().enableGroup(groupName);
        // Cannot know wich feature to work with (exceptional event) : flush cache
        getCacheManager().clearFeatures();
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
        getTargetFeatureStore().disableGroup(groupName);
        // Cannot know wich feature to work with (exceptional event) : flush cache
        getCacheManager().clearFeatures();
    }

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        // Cache cannot help you
        return getTargetFeatureStore().existGroup(groupName);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        // Cache cannot help you
        return getTargetFeatureStore().readGroup(groupName);
    }

    /** {@inheritDoc} */
    @Override
    public void addToGroup(String featureId, String groupName) {
        getTargetFeatureStore().addToGroup(featureId, groupName);
        getCacheManager().evictFeature(featureId);
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String featureId, String groupName) {
        getTargetFeatureStore().removeFromGroup(featureId, groupName);
        getCacheManager().evictFeature(featureId);
    }

    /**
     * Getter accessor for attribute 'target'.
     * 
     * @return current value of 'target'
     */
    public FeatureStore getTargetFeatureStore() {
        if (targetFeatureStore == null) {
            throw new IllegalArgumentException("ff4j-core: Target for cache proxy has not been provided");
        }
        return targetFeatureStore;
    }

    /**
     * Getter accessor for attribute 'target'.
     * 
     * @return current value of 'target'
     */
    public PropertyStore getTargetPropertyStore() {
        if (targetPropertyStore == null) {
            throw new IllegalArgumentException("ff4j-core: Target for cache proxy has not been provided");
        }
        return targetPropertyStore;
    }

    /**
     * Getter accessor for attribute 'cacheManager'.
     * 
     * @return current value of 'cacheManager'
     */
    public FF4JCacheManager getCacheManager() {
        if (cacheManager == null) {
            throw new IllegalArgumentException("ff4j-core: CacheManager for cache proxy has not been provided but it's required");
        }
        return cacheManager;
    }

    /**
     * Setter accessor for attribute 'cacheManager'.
     * 
     * @param cacheManager
     *            new value for 'cacheManager '
     */
    public void setCacheManager(FF4JCacheManager cacheManager) {
        this.cacheManager = cacheManager;
    }

    // ------------ Cache related method --------------------

    /** {@inheritDoc} */
    public boolean isCached() {
        return true;
    }

    /** {@inheritDoc} */
    public String getCacheProvider() {
        if (cacheManager != null) {
            return cacheManager.getCacheProviderName();
        } else {
            return null;
        }
    }

    /** {@inheritDoc} */
    public String getCachedTargetStore() {
        return getTargetFeatureStore().getClass().getCanonicalName();
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Property<?>> readAllProperties() {
        return getTargetPropertyStore().readAllProperties();
    }

    /** {@inheritDoc} */
    @Override
    public boolean existProperty(String propertyName) {
        // not in cache but maybe created from last access
        if (getCacheManager().getProperty(propertyName) == null) {
            return getTargetPropertyStore().existProperty(propertyName);
        }
        return true;
    }

    /** {@inheritDoc} */
    @Override
    public <T> void createProperty(Property<T> property) {
        getTargetPropertyStore().createProperty(property);
        getCacheManager().putProperty(property);
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> readProperty(String name) {
        Property<?> fp = getCacheManager().getProperty(name);
        // not in cache but may has been created from now
        if (null == fp) {
            fp = getTargetPropertyStore().readProperty(name);
            getCacheManager().putProperty(fp);
        }
        return fp;
    }

    /** {@inheritDoc} */
    @Override
    public void updateProperty(String name, String newValue) {
        // Retrieve the full object from its name
        Property<?> fp = getTargetPropertyStore().readProperty(name);
        fp.setValueFromString(newValue);
        // Update value in target store
        getTargetPropertyStore().updateProperty(fp);
        // Remove from cache old value
        getCacheManager().evictProperty(fp.getName());
        // Add new value in the cache
        getCacheManager().putProperty(fp);
    }

    /** {@inheritDoc} */
    @Override
    public <T> void updateProperty(Property<T> propertyValue) {
        // Update the property
        getTargetPropertyStore().updateProperty(propertyValue);
        // Update the cache accordirly
        getCacheManager().evictProperty(propertyValue.getName());
        // Update the property in cache
        getCacheManager().putProperty(propertyValue);
    }

    /** {@inheritDoc} */
    @Override
    public void deleteProperty(String name) {
        // Access target store
        getTargetPropertyStore().deleteProperty(name);
        // even is not present, evict name failed
        getCacheManager().evictProperty(name);
    }

    /** {@inheritDoc} */
    @Override
    public boolean isEmpty() {
        Set<String> pNames = listPropertyNames();
        return pNames != null && !pNames.isEmpty();
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> listPropertyNames() {
        return getTargetPropertyStore().listPropertyNames();
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        // Cache Operations : As modification, flush cache for this
        getCacheManager().clearProperties();
        getTargetPropertyStore().clear();

        // Cache Operations : As modification, flush cache for this
        getCacheManager().clearFeatures();
        getTargetFeatureStore().clear();
    }

    /**
     * Setter accessor for attribute 'targetFeatureStore'.
     * 
     * @param targetFeatureStore
     *            new value for 'targetFeatureStore '
     */
    public void setTargetFeatureStore(FeatureStore targetFeatureStore) {
        this.targetFeatureStore = targetFeatureStore;
    }

    /**
     * Setter accessor for attribute 'targetPropertyStore'.
     * 
     * @param targetPropertyStore
     *            new value for 'targetPropertyStore '
     */
    public void setTargetPropertyStore(PropertyStore targetPropertyStore) {
        this.targetPropertyStore = targetPropertyStore;
    }

}
