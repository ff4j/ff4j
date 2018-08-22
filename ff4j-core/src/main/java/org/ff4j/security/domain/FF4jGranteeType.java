package org.ff4j.security.domain;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2018 FF4J
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
 * Single Table of permission, ;ultiple targets.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public enum FF4jGranteeType {
    
    /** Higher level **/
    FF4J,
    
    /** Permission is related to a Feature. */
    FEATURE,
    
    /** Permission is related to a Property. */
    PROPERTY,
    
    /** Permisison is part of a role. */
    ROLE,
    
    /** Feature Store. */
    FEATURE_STORE,
    
    /** Feature Store. */
    PROPERTY_STORE,
    
    /** Web console if relevant. */
    WEB_UI,
    
    /** CRUD operations. */
    REST_API,
    
    /** JMX CRUD operations. */
    JMX_API,
    
    /** Let people add their own if relevant for them. */
    CUSTOM;

}
