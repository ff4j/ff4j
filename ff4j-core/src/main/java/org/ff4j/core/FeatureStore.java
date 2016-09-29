package org.ff4j.core;

import java.util.Collection;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 Ff4J
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

/**
 * Repository to persist {@link Feature}(s)
 * 
 * @author Cedrick Lunven (@clunven)
 */
public interface FeatureStore {

    /**
     * Enable/UP/switch on a FlipPoint.
     * 
     * @param featureID
     *            unique feature identifier
     */
    void enable(String featureID);

    /**
     * Create Feature is does not exist and enable it.
     * 
     * @param featureID
     *            unique feature identifier
     */
    void disable(String fId);

    /**
     * Check if Feature Exist.
     * 
     * @return unique feature identifier
     */
    boolean exist(String featId);

    /**
     * Create flipPoint in storage (with roles).
     * 
     * @param fp
     *            create roles
     */
    void create(Feature fp);

    /**
     * Create Feature is does not exist and enable it.
     * 
     * @param featureID
     *            unique feature identifier
     */
    Feature read(String featureUid);

    /**
     * Access to all features to get information.
     * 
     * @return all features
     */
    Map<String, Feature> readAll();

    /**
     * Remove fliPoint from store.
     * 
     * @param fp
     *            flipPoint
     */
    void delete(String fpId);

    /**
     * Update FlipPoint within store.
     * 
     * @param fp
     *            flipPoint new element
     */
    void update(Feature fp);

    /**
     * Add a role to a flipPOINT.
     * 
     * @param roleName
     */
    void grantRoleOnFeature(String flipId, String roleName);

    /**
     * Remove role to acess flip point
     * 
     * @param roleName
     */
    void removeRoleFromFeature(String flipId, String roleName);

    /**
     * Enable all features contained in the following group.
     * 
     * @param groupName
     *            target group name
     */
    void enableGroup(String groupName);

    /**
     * Disable all features contained in the following group.
     * 
     * @param groupName
     *            target group name
     */
    void disableGroup(String groupName);

    /**
     * Check if current group exist or not.
     * 
     * @param groupName
     *            target group name
     */
    boolean existGroup(String groupName);

    /**
     * Read all features within target group.
     * 
     * @param groupName
     *            target group name
     * @return return all feature from group or groupnotfoundException if does not exist
     */
    Map<String, Feature> readGroup(String groupName);
    
    /**
     * Add target {@link Feature} to target group.
     * 
     * @param featureId
     *            target feature identifier
     * @param groupName
     *            target groupName
     */
    void addToGroup(String featureId, String groupName);
    
    /**
     * Remove target {@link Feature} from group.
     * 
     * @param featureId
     *            target feature identifier
     * @param groupName
     *            target groupName
     */
    void removeFromGroup(String featureId, String groupName);
    
    /**
     * Return a set of existing groups.
     * 
     * @return set of group in the store
     */
    Set<String> readAllGroups();
    
    /**
     * Empty features set.
     */
    void clear();
    
    /**
     * Import features.
     *
     * @param features
     *      list of features.s
     */
    void importFeatures(Collection < Feature > features);
    
    /**
     * Initialize the target database schema by creating expected structures.
     * 
     * <li> TABLE, INDEX will be created for JDBC, but also COLLECTION and INDEXS for MongoDb, or COLUMN FAMILY for Cassandra.
     * <li> The structures will be created only if they don't exist.
     * <li> In some cases, there is nothing todo (Ehcache, Redis, InMemory), the method won't failed but do nothing (it does not clear the DB) 
     * 
     * @since 1.6
     */
    void createSchema();
    
}
