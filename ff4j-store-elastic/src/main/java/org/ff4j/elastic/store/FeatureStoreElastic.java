package org.ff4j.elastic.store;

import java.util.Map;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.elastic.ElasticConnection;
import org.ff4j.store.AbstractFeatureStore;

/*
 * #%L
 * ff4j-store-elastic
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


public class FeatureStoreElastic extends AbstractFeatureStore {
    
    private ElasticConnection connection;

    /** {@inheritDoc} */
    @Override
    public void enable(String featureID) {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String fId) {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String featId) {
        // TODO Auto-generated method stub
        return false;
    }

    /** {@inheritDoc} */
    @Override
    public void create(Feature fp) {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String featureUid) {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String fpId) {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String flipId, String roleName) {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String flipId, String roleName) {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    @Override
    public void enableGroup(String groupName) {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        // TODO Auto-generated method stub
        return false;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void addToGroup(String featureId, String groupName) {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String featureId, String groupName) {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> readAllGroups() {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        // TODO Auto-generated method stub
        
    }

    /**
     * Getter accessor for attribute 'connection'.
     *
     * @return
     *       current value of 'connection'
     */
    public ElasticConnection getConnection() {
        return connection;
    }

    /**
     * Setter accessor for attribute 'connection'.
     * @param connection
     * 		new value for 'connection '
     */
    public void setConnection(ElasticConnection connection) {
        this.connection = connection;
    }

}
