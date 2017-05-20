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
import java.util.Map;

import org.ff4j.core.Feature;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
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
