package org.ff4j.security.domain;

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

/**
 * Access Control List on FF4j Capabilities.
 *
 * @author Cedrick LUNVEN  (@clunven)
 */
public enum FF4jPermission {
    
    // ADMIN >> TOGGLE >> VIEW
    
    // (ff4j) Can edit all features independently of any ACL on the entities
    ADMIN_FEATURES,

    // View & can toggle on/off a feature
    TOGGLE_FEATURES,
    
    // View Features
    VIEW_FEATURES,
    
    // (ff4j) Can edit all features independently of any ACL on the entities
    ADMIN_PROPERTIES,
    
    // View properties
    VIEW_PROPERTIES,
    
    // Can access and search audit
    VIEW_AUDITTRAIL,

    // Can access and search audit
    VIEW_FEATUREUSAGE,
    
    // --------------- FEATURE -----------------------------------------------
    
    // (feature) Can edit feature (CREATE, DELETE, UPDATE, TOGGLE)
    FEATURE_ADMIN,
    
    // (feature) You cannot edit feature but toggleOn/Off. (Business Users)
    FEATURE_TOGGLE,
    
    // (feature) You vue status and browse monitoring. (Business Users)
    FEATURE_VIEW,
    
    // --------------- PROPERTY -----------------------------------------------
    
    // (property) Can edit feature (CREATE, DELETE, UPDATE, TOGGLE)
    PROPERTY_ADMIN,
    
    // You can edit properties
    PROPERTY_VIEW,
    
}
