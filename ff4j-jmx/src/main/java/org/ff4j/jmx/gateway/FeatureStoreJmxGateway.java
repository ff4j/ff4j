package org.ff4j.jmx.gateway;

import java.util.Map;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jmx.export.annotation.ManagedOperation;
import org.springframework.jmx.export.annotation.ManagedOperationParameter;
import org.springframework.jmx.export.annotation.ManagedOperationParameters;

/*
 * #%L
 * ff4j-jmx
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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

/**
 * Allow to process features logic through JMX.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
//@Component
//@ManagedResource(objectName = "org.ff4j.jmx:type=FeatureStoreJmx")
public class FeatureStoreJmxGateway implements FeatureStore {

    @Autowired
    private FeatureStore internalStore;
    
    /** {@inheritDoc} */
    @Override
    @ManagedOperation(description = "Enable feature from its identifier")
    @ManagedOperationParameters({@ManagedOperationParameter(name = "feature_UID", description = "Identifier of feature to enable")})
    public void enable(String feature_UID) {
        internalStore.enable(feature_UID);
    }

    /** {@inheritDoc} */
    @Override
    @ManagedOperation(description = "Disable feature from its identifier")
    @ManagedOperationParameters({@ManagedOperationParameter(name = "feature_UID", description = "Identifier of feature to disable")})
    public void disable(String feature_UID) {
        internalStore.disable(feature_UID);
    }

    /** {@inheritDoc} */
    @Override
    @ManagedOperation(description = "Test if a feature exists based on its identifier")
    @ManagedOperationParameters({@ManagedOperationParameter(name = "feature_UID", description = "Identifier of feature to test")})
    public boolean exist(String feature_UID) {
        return internalStore.exist(feature_UID);
    }

    /** {@inheritDoc} */
    @Override
    @ManagedOperation(description = "Create Feature")
    @ManagedOperationParameters({@ManagedOperationParameter(name = "feature_UID", description = "Identifier of feature to enable")})
    public void create(Feature fp) {
        internalStore.create(fp);
    }

    /** {@inheritDoc} */
    @Override
    @ManagedOperation(description = "Disable feature from its identifier")
    @ManagedOperationParameters({@ManagedOperationParameter(name = "feature_UID", description = "Unique Identifier of feature")})
    public Feature read(String feature_UID) {
        return internalStore.read(feature_UID);
    }

    @Override
    public Map<String, Feature> readAll() {
        return null;
    }

    @Override
    public void delete(String fpId) {
    }

    @Override
    public void update(Feature fp) {    }

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
