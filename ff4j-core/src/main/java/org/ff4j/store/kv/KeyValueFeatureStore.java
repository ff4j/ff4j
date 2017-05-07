package org.ff4j.store.kv;

/*
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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.mapper.FeatureMapper;
import org.ff4j.store.AbstractFeatureStore;
import org.ff4j.utils.Util;

/**
 * Superclass to work with Key/Value Stores.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public abstract class KeyValueFeatureStore < VALUE > extends AbstractFeatureStore {

    /** Driver to access a K/V Store. */
    protected KeyValueDriver < String, VALUE > driver;
    
    /** Work with Mapping. */
    protected FeatureMapper < VALUE > featureMapper;
    
    /**
     * Default constructor
     */
    public KeyValueFeatureStore() {
    }
            
    /**
     * Work with Key-Value.
     *
     * @param driver
     *      target driver
     */
    public KeyValueFeatureStore(KeyValueDriver< String, VALUE > driver) {
        this.driver = driver;
    }
    
    /**
     * Work with Key-Value.
     *
     * @param driver
     *      target driver
     */
    public KeyValueFeatureStore(KeyValueDriver< String, VALUE > driver, FeatureMapper < VALUE > mapper) {
        this.driver = driver;
        this.featureMapper = mapper;
    }
        
    /** {@inheritDoc} */
    @Override
    public boolean exist(String uid) {
        Util.assertParamHasLength(uid, "Feature identifier");
        // or : getDriver().getFeatureList().contains(uid)
        return getDriver().existKey(getDriver().getFeatureKey(uid));
    }
    
    /** {@inheritDoc} */
    @Override
    public void enable(String uid) {
        // Read from redis, feature not found if no present
        Feature f = read(uid);
        // Update within Object
        f.enable();
        // Serialization and update key, update TTL
        update(f);
    }
    
    /** {@inheritDoc} */
    @Override
    public void disable(String uid) {
        // Read from redis, feature not found if no present
        Feature f = read(uid);
        // Update within Object
        f.disable();
        // Serialization and update key, update TTL
        update(f);
    }
    
    /** {@inheritDoc} */
    @Override
    public void update(Feature feature) {
        if (feature == null) {
            throw new IllegalArgumentException("Feature cannot be null");
        }
        if (!exist(feature.getUid())) {
            throw new FeatureNotFoundException(feature.getUid());
        }
        getDriver().putValue(
                getDriver().getFeatureKey(feature.getUid()), 
                getFeatureMapper().toStore(feature));
    }
    
    /** {@inheritDoc} */
    @Override
    public Feature read(String uid) {
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        return getFeatureMapper().fromStore(
                getDriver().getValue(getDriver().getFeatureKey(uid)));
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        Map < String, Feature> mapOfFeatures = new HashMap<String, Feature>();
        for(String featureName : getDriver().getFeatureList()) {
            Feature currF = getFeatureMapper().fromStore(getDriver().getValue(
                                getDriver().getFeatureKey(featureName)));
            mapOfFeatures.put(currF.getUid(), currF);
        }
        return mapOfFeatures;
    }   
    
    /** {@inheritDoc} */
    @Override
    public void create(Feature feature) {
        if (feature == null) {
            throw new IllegalArgumentException("Feature cannot be null nor empty");
        }
        if (exist(feature.getUid())) {
            throw new FeatureAlreadyExistException(feature.getUid());
        }
        // Create feature key
        getDriver().putValue(
                getDriver().getFeatureKey(feature.getUid()), 
                getFeatureMapper().toStore(feature));
        // Register in the dictionnary
        getDriver().registerFeature(feature.getUid());
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String uid) {
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        // Delete feature key
        getDriver().deleteKey(getDriver().getFeatureKey(uid));
        // Register in the dictionnary
        getDriver().unregisterFeature(uid);
    }  
    
    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String flipId, String roleName) {
        Util.assertParamHasLength(roleName, "roleName (#2)");
        // retrieve
        Feature f = read(flipId);
        // modify
        f.getPermissions().add(roleName);
        // persist modification
        update(f);
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String flipId, String roleName) {
        Util.assertParamHasLength(roleName, "roleName (#2)");
        // retrieve
        Feature f = read(flipId);
        f.getPermissions().remove(roleName);
        // persist modification
        update(f);
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        Util.assertParamHasLength(groupName, "groupName");
        Map < String, Feature > features = readAll();
        Map < String, Feature > group = new HashMap< String, Feature >();
        for (Map.Entry<String,Feature> uid : features.entrySet()) {
            if (groupName.equals(uid.getValue().getGroup())) {
                group.put(uid.getKey(), uid.getValue());
            }
        }
        if (group.isEmpty()) {
            throw new GroupNotFoundException(groupName);
        }
        return group;
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        Util.assertParamHasLength(groupName, "groupName");
        Map < String, Feature > features = readAll();
        Map < String, Feature > group = new HashMap< String, Feature >();
        for (Map.Entry<String,Feature> uid : features.entrySet()) {
            if (groupName.equals(uid.getValue().getGroup())) {
                group.put(uid.getKey(), uid.getValue());
            }
        }
        return !group.isEmpty();
    }

    /** {@inheritDoc} */
    @Override
    public void enableGroup(String groupName) {
        Map < String, Feature > features = readGroup(groupName);
        for (Map.Entry<String,Feature> uid : features.entrySet()) {
            uid.getValue().enable();
            update(uid.getValue());
        }
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
        Map < String, Feature > features = readGroup(groupName);
        for (Map.Entry<String,Feature> uid : features.entrySet()) {
            uid.getValue().disable();
            update(uid.getValue());
        }
    }

    /** {@inheritDoc} */
    @Override
    public void addToGroup(String featureId, String groupName) {
        Util.assertParamHasLength(groupName, "groupName (#2)");
        // retrieve
        Feature f = read(featureId);
        f.setGroup(groupName);
        // persist modification
        update(f);
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String featureId, String groupName) {
        Util.assertParamHasLength(groupName, "groupName (#2)");
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        // retrieve
        Feature f = read(featureId);
        f.setGroup(null);
        // persist modification
        update(f);
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> readAllGroups() {
        Map < String, Feature > features = readAll();
        Set < String > groups = new HashSet<String>();
        for (Map.Entry<String,Feature> uid : features.entrySet()) {
            groups.add(uid.getValue().getGroup());
        }
        groups.remove(null);
        return groups;
    }
    
    /** {@inheritDoc} */
    @Override
    public void clear() {
        // N+1 select is faster (N+1).log(N) than full scan (N^2) 
        for (String uid : getDriver().getFeatureList()) {
            delete(uid);
        }
    }

    /**
     * Getter accessor for attribute 'driver'.
     *
     * @return
     *       current value of 'driver'
     */
    public KeyValueDriver< String, VALUE > getDriver() {
        if (driver == null) {
            throw new IllegalStateException("Cannot access target K/V driver, please initialize");
        }
        return driver;
    }

    /**
     * Setter accessor for attribute 'driver'.
     * @param driver
     * 		new value for 'driver '
     */
    public void setDriver(KeyValueDriver< String, VALUE > driver) {
        this.driver = driver;
    }

    /**
     * Getter accessor for attribute 'featureMapper'.
     *
     * @return
     *       current value of 'featureMapper'
     */
    public FeatureMapper < VALUE > getFeatureMapper() {
        if (featureMapper == null) {
            throw new IllegalStateException("Please initialize feature mapper");
        }
        return featureMapper;
    }

    /**
     * Setter accessor for attribute 'featureMapper'.
     * @param featureMapper
     * 		new value for 'featureMapper '
     */
    public void setFeatureMapper(FeatureMapper<VALUE> featureMapper) {
        this.featureMapper = featureMapper;
    }
}
