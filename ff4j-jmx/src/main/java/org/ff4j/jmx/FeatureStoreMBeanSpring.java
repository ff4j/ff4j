package org.ff4j.jmx;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.springframework.jmx.export.annotation.ManagedOperation;
import org.springframework.jmx.export.annotation.ManagedOperationParameter;
import org.springframework.jmx.export.annotation.ManagedOperationParameters;
import org.springframework.jmx.export.annotation.ManagedResource;
import org.springframework.stereotype.Component;

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
@Component
@ManagedResource(objectName = "org.ff4j.jmx:type=FeatureStoreMBeanSpring")
public class FeatureStoreMBeanSpring implements FeatureStore {

    /** Target underlying store. */
    //@Autowired
    private FeatureStore internalStore;
    
    /** {@inheritDoc} */
    @Override
    @ManagedOperation(description = "Enable a feature")
    @ManagedOperationParameters({@ManagedOperationParameter(name = "uid", description = "Feature unique id")})
    public void enable(String uid) {
        internalStore.enable(uid);
    }

    /** {@inheritDoc} */
    @Override
    @ManagedOperation(description = "Disable a feature")
    @ManagedOperationParameters({@ManagedOperationParameter(name = "uid",  description = "Feature unique id")})
    public void disable(String uid) {
        internalStore.disable(uid);
    }

    @Override
    @ManagedOperation(description = "Test if a feature exists based on its identifier")
    @ManagedOperationParameters({@ManagedOperationParameter(name = "feature_UID", description = "Identifier of feature to test")})
    public boolean exist(String feature_UID) {
        return internalStore.exist(feature_UID);
    }

    @Override
    @ManagedOperation(description = "Create Feature")
    @ManagedOperationParameters({@ManagedOperationParameter(name = "feature_UID", description = "Identifier of feature to enable")})
    public void create(Feature fp) {
        internalStore.create(fp);
    }

    @Override
    @ManagedOperation(description = "Disable feature from its identifier")
    @ManagedOperationParameters({@ManagedOperationParameter(name = "feature_UID", description = "Unique Identifier of feature")})
    public Feature read(String feature_UID) {
        return internalStore.read(feature_UID);
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
    public void importFeatures(Collection<Feature> features) {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        // TODO Auto-generated method stub
        
    }
    
}
