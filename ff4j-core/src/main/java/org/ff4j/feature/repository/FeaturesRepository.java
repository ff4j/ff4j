package org.ff4j.feature.repository;

/*-
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

import java.util.stream.Stream;

import org.ff4j.FF4jRepository;
import org.ff4j.exception.ItemNotFoundException;
import org.ff4j.feature.Feature;
import org.ff4j.feature.exception.FeatureNotFoundException;
import org.ff4j.feature.exception.GroupNotFoundException;

/**
 * Repository to persist {@link Feature}(s)
 * 
 * @author Cedrick Lunven (@clunven)
 */
public interface FeaturesRepository extends FF4jRepository < String, Feature > {
    
    /**
     * Toggle ON a feature by its identifier.
     * 
     * @param uid
     *            unique feature identifier
     */
    void toggleOn(String uid);

    /**
     * Toggle off a feature by its identifier.
     * 
     * @param uid
     *            unique feature identifier
     */
    void toggleOff(String uid);
    
    /**
     * Syntax sugar.
     *
     * @return
     *      list of features names.
     */
    default Stream < String > listFeatureNames() {
        return findAllIds();
    }
    
    // ---------------------------------
    // --------- GROUPS  ---------------
    // ---------------------------------
   
    /**
     * Check if current group exist or not.
     * 
     * @param groupName
     *            target group name
     */
    boolean existGroup(String groupName);
    
    /**
     * Enable all features related to the parameter group
     * 
     * @param groupName
     *            target group name
     */
    void toggleOnGroup(String groupName);

    /**
     * Disable all features related to the parameter group
     * 
     * @param groupName
     *            target group name
     */
    void toggleOffGroup(String groupName);

    /**
     * Read all features within target group.
     * 
     * @param groupName
     *            target group name
     * @return return all feature from group or groupnotfoundException if does not exist
     * 
     * @throws GroupNotFoundException
     *              if group does not exist
     */
    Stream < Feature> readGroup(String groupName);
    
    /**
     * Add target {@link Feature} to target group.
     * 
     * @param uid
     *            target feature identifier
     * @param groupName
     *            target groupName
     */
    void addToGroup(String uid, String groupName);
    
    /**
     * Remove target {@link Feature} from group.
     * 
     * @param uid
     *            target feature identifier
     * @param groupName
     *            target groupName
     */
    void removeFromGroup(String uid, String groupName);
    
    /**
     * Return a set of existing groups.
     * 
     * @return set of group in the store
     */
    Stream < String > listGroupNames();
    
    /**
     * Specialization of 'update' without any concern about audit nor cache.
     *
     * @param fp
     *      target feature
     */
    void saveFeature(Feature feature);
    
    /**
     * Specialization of 'delete' without any concern about audit nor cache.
     *
     * @param fp
     *      target feature
     */
    void deleteFeature(String uid);
    
    /**
     * Specializing exception as {@link FeatureNotFoundException}
     */
    @Override
    default Feature read(String uid) {
        try {
            return FF4jRepository.super.read(uid);
        } catch(ItemNotFoundException inf) {
            throw new FeatureNotFoundException(uid);
        }
    }
    
}
