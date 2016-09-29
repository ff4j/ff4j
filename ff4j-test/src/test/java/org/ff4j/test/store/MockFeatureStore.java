package org.ff4j.test.store;

import java.util.Collection;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Mock implementation of {@link FeatureStore} to make tests.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class MockFeatureStore implements FeatureStore {
	
	/** Logger. */
	private static Logger LOGGER = LoggerFactory.getLogger(MockFeatureStore.class);

	/** Change behaviour at runtime. */
    private boolean empty = false;
    
    /** {@inheritDoc} */
    public void enable(String featureID) {
    	LOGGER.debug("MOCK [enable]" + featureID);
    }

    /** {@inheritDoc} */
    public void disable(String featureID) {
    	LOGGER.debug("MOCK [disable]" + featureID);
    }

    /** {@inheritDoc} */
    public boolean exist(String featId) {
        return ("first".equals(featId));
    }

    /** {@inheritDoc} */
    public void create(Feature fp) {
    	LOGGER.debug("MOCK [create]" + fp);
    }

    /** {@inheritDoc} */
    public Feature read(String featureUid) {
        if ("first".equals(featureUid)) return new Feature("first");
        return null;
    }

    /** {@inheritDoc} */
    public Map<String, Feature> readAll() {
        Map < String, Feature> map = new HashMap<String, Feature>();
        if (!empty) {
            map.put("a", new Feature("a", true));
        }
        return map;
    }
    
    /** {@inheritDoc} */
    public void delete(String featureID) {
    	LOGGER.debug("MOCK [delete]" + featureID);
    }
    
    /** {@inheritDoc} */
    public void update(Feature fp) {
    	LOGGER.debug("MOCK [update]" + fp);
    }
    
    /** {@inheritDoc} */
    public void grantRoleOnFeature(String flipId, String roleName) {
    	LOGGER.debug("MOCK [grantRoleOnFeature]" + flipId);
    }
    
    /** {@inheritDoc} */
    public void removeRoleFromFeature(String flipId, String roleName) {
    	LOGGER.debug("MOCK [removeRoleFromFeature]" + flipId);
    }
    
    /** {@inheritDoc} */
    public void enableGroup(String groupName) {
    	LOGGER.debug("MOCK [enableGroup]" + groupName);
    }
    
    /** {@inheritDoc} */
    public void disableGroup(String groupName) {
    	LOGGER.debug("MOCK [disableGroup]" + groupName);
    }
    
    /** {@inheritDoc} */
    public boolean existGroup(String groupName) {
        if ("GRP1".equals(groupName)) return true;
        return false;
    }
    
    /** {@inheritDoc} */
    public Map<String, Feature> readGroup(String groupName) {
    	LOGGER.debug("MOCK [readGroup]" + groupName);
        return null;
    }
    
    /** {@inheritDoc} */
    public void addToGroup(String featureId, String groupName) {
    	LOGGER.debug("MOCK [addToGroup]" + featureId);
    }
    
    /** {@inheritDoc} */
    public void removeFromGroup(String featureId, String groupName) {
    	LOGGER.debug("MOCK [removeFromGroup]" + featureId);
    }
    
    /** {@inheritDoc} */
    public Set<String> readAllGroups() {
    	LOGGER.debug("MOCK [readAllGroups]");
        return null;
    }

    /** {@inheritDoc} */
    public void clear() {
        empty = true;
    }

    /** {@inheritDoc} */
    public void importFeatures(Collection<Feature> features) {
        LOGGER.debug("MOCK [importFeatures]");
    }

    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        LOGGER.debug("MOCK [createSchema]");
    }

}
