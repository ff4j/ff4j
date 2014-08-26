package org.ff4j.store;

/*
 * #%L
 * ff4j-store-redis
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

import java.util.Map;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;

public class FeatureStoreRedis  implements FeatureStore {

    /** {@inheritDoc} */
    @Override
    public void enable(String featureID) {
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String fId) {
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String featId) {
        return false;
    }

    /** {@inheritDoc} */
    @Override
    public void create(Feature fp) {
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String featureUid) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String fpId) {
    }

    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
    }

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String flipId, String roleName) {
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String flipId, String roleName) {
    }

    /** {@inheritDoc} */
    @Override
    public void enableGroup(String groupName) {
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
    }

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
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
    public boolean isCached() {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public String getCacheProvider() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String getCachedTargetStore() {
        // TODO Auto-generated method stub
        return null;
    }
    

}
