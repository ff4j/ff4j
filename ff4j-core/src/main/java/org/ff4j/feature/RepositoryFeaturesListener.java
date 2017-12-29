package org.ff4j.feature;

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

import org.ff4j.FF4jEntity;
import org.ff4j.repository.FF4jRepositoryListener;

/**
 * Public Interface of a Listener on CRUD repository.
 * @Do not put any onRead() as making not sense in ff4J.
 * 
 * @author Cedrick LUNVEN  (@clunven)
 *
 * @param <ENTITY>
 *    {@link FF4jEntity} to be specialized by type of store 
 */
public interface RepositoryFeaturesListener extends FF4jRepositoryListener< Feature > {
    
    void onToggleOnFeature(String uid);
    
    void onToggleOffFeature(String uid);
    
    void onToggleOnGroup(String groupName);
    
    void onToggleOffGroup(String groupname);
    
    void onAddFeatureToGroup(String uid, String groupName);
    
    void onRemoveFeatureFromGroup(String uid, String groupName);
    
}
