package org.ff4j.services.constants;

/*
 * #%L
 * ff4j-spring-services
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

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public final class FeatureConstants {

    // PATH PARAM
    public static final String PATH_PARAM_GROUP = "{groupName}";
    public static final String PATH_PARAM_UID   = "{uid}";
    public static final String PATH_PARAM_ROLE  = "{role}";
    public static final String PATH_PARAM_NAME  = "{name}";
    public static final String PATH_PARAM_VALUE = "{value}";
    
    // PARAM
    public static final String PARAM_ROLE       = "role";
    public static final String PARAM_GROUP      = "groupName";
    public static final String PARAM_NAME       = "name";
    public static final String PARAM_VALUE      = "value";
    
    // RESOURCE
    public static final String ROOT = "/api/";
    
    public static final String RESOURCE_FF4J                = ROOT + "ff4j";
    
    public static final String RESOURCE_STORE               = "/store";
    public static final String RESOURCE_FF4J_STORE = RESOURCE_FF4J + RESOURCE_STORE;
    
    public static final String RESOURCE_FEATURES            = "/features";
    public static final String RESOURCE_FF4J_STORE_FEATURES = RESOURCE_FF4J + RESOURCE_STORE + RESOURCE_FEATURES;
    
    public static final String RESOURCE_GROUPS              = "/groups";
    public static final String RESOURCE_FF4J_STORE_GROUPS   = RESOURCE_FF4J + RESOURCE_STORE + RESOURCE_GROUPS;
    
    public static final String RESOURCE_PROPERTY_STORE      = "/propertyStore";
    public static final String RESOURCE_PROPERTIES          = "/properties";
    public static final String RESOURCE_PROPERTIES_STORE_PROPERTIES = RESOURCE_FF4J + RESOURCE_PROPERTY_STORE + RESOURCE_PROPERTIES;
    
    public static final String RESOURCE_FF4J_PROPERTY_STORE = RESOURCE_FF4J + RESOURCE_PROPERTY_STORE;
    public static final String RESOURCE_CLEAR_CACHE = "/clearCache";
    public static final String RESOURCE_FF4J_MONITORING = RESOURCE_FF4J + "/monitoring";

    private FeatureConstants() {
        throw new UnsupportedOperationException();
    }
}
