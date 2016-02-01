package org.ff4j.test.store;

import java.util.HashMap;

/*
 * #%L
 * ff4j-test
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
import org.ff4j.core.FeatureStore;

public class MockFeatureStore implements FeatureStore {

    private boolean empty = false;
    
    @Override
    public void enable(String featureID) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void disable(String fId) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public boolean exist(String featId) {
        if ("first".equals(featId)) return true;
        
        return false;
    }

    @Override
    public void create(Feature fp) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public Feature read(String featureUid) {
        if ("first".equals(featureUid)) return new Feature("first");
        return null;
    }

    @Override
    public Map<String, Feature> readAll() {
        Map < String, Feature> map = new HashMap<String, Feature>();
        if (!empty) {
            map.put("a", new Feature("a", true));
        }
        return map;
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
        if ("GRP1".equals(groupName)) return true;
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
        empty = true;;
        
    }

}
