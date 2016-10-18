package org.ff4j.lab;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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

import java.util.Map;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.store.AbstractFeatureStore;

/**
 * Leverage on {@link PropertyStore} to handle features. Feature will
 * be represented through its JSON representation.
 *  
 * @author Cedrick LUNVEN (@clunven)
 */
public class FeatureStoreOverPropertyStore extends  AbstractFeatureStore {
    
    /** Emebbeded property store. */
    private PropertyStore propertyStore = null;
    
    /**
     * Default public constructor.
     */
    public FeatureStoreOverPropertyStore() {
    }
    
    /**
     * Recopy constructor
     *
     * @param pStore
     *      target store
     */
    public FeatureStoreOverPropertyStore(PropertyStore pStore) {
        this.propertyStore = pStore;
    }

    /**
     * Getter accessor for attribute 'propertyStore'.
     *
     * @return
     *       current value of 'propertyStore'
     */
    public PropertyStore getPropertyStore() {
        if (this.propertyStore == null) {
            throw new IllegalStateException("Cannot read feature as propertystore has not been provided");
        }
        return propertyStore;
    }

    /**
     * Setter accessor for attribute 'propertyStore'.
     * @param propertyStore
     * 		new value for 'propertyStore '
     */
    public void setPropertyStore(PropertyStore propertyStore) {
        this.propertyStore = propertyStore;
    }

    /** {@inheritDoc} */
    @Override
    public void enable(String uid) {
        assertFeatureExist(uid);
        PropertyFeature prop = (PropertyFeature) propertyStore.readProperty(uid);
        prop.getValue().enable();
        propertyStore.updateProperty(uid, prop.getValue().toJson());
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String uid) {
        assertFeatureExist(uid);
        PropertyFeature prop = (PropertyFeature) propertyStore.readProperty(uid);
        prop.getValue().disable();
        propertyStore.updateProperty(uid, prop.getValue().toJson());
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String uid) {
        return propertyStore.existProperty(uid);
    }

    /** {@inheritDoc} */
    @Override
    public void create(Feature feature) {
        assertFeatureNotNull(feature);
        assertFeatureNotExist(feature.getUid());
        propertyStore.createProperty(new PropertyFeature(feature));
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String uid) {
        
        return null;
    }

    @Override
    public Map<String, Feature> readAll() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void delete(String fpId) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void update(Feature fp) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void grantRoleOnFeature(String flipId, String roleName) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void removeRoleFromFeature(String flipId, String roleName) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void enableGroup(String groupName) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void disableGroup(String groupName) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public boolean existGroup(String groupName) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public Map<String, Feature> readGroup(String groupName) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void addToGroup(String featureId, String groupName) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void removeFromGroup(String featureId, String groupName) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public Set<String> readAllGroups() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void clear() {
        // TODO Auto-generated method stub
        
    }
}
