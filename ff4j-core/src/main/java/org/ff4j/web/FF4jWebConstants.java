package org.ff4j.web;

/*
 * #%L
 * ff4j-web
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
 * Constants used in the FF4J RESTFul Api Definition.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class FF4jWebConstants {

    /** Header param sent on creation 201. */
    public static final String LOCATION = "Location";

    /** expected post parameter from POST methods. */
    public static final String POST_PARAMNAME_FEATURE_UID = "uid";
    
    /** parameter. */
    public static final String POST_PARAMNAME_CUSTOM_PREFIX = "PARAM_";

    /** Custom operation on resource. */
    public static final String OPERATION_ENABLE = "enable";

    /** Custom operation on resource. */
    public static final String OPERATION_DISABLE = "disable";

    /** Custom operation on resource. */
    public static final String OPERATION_CHECK = "check";

    /** Custom operation on resource. */
    public static final String OPERATION_GRANTROLE = "grantrole";

    /** Custom operation on resource. */
    public static final String OPERATION_REMOVEROLE = "removerole";

    /** Custom operation on resource. */
    public static final String OPERATION_ADDGROUP = "addGroup";

    /** Custom operation on resource. */
    public static final String OPERATION_REMOVEGROUP = "removeGroup";

    /** Custom operation on resource. */
    public static final String OPERATION_UPDATE = "update";
    
    /** relative path. */
    public static final String RESOURCE_FEATURES = "features";
    
    /** relative path. */
    public static final String RESOURCE_PROPERTIES = "properties";

    /** relative path. */
    public static final String RESOURCE_GROUPS = "groups";

    /** relative path. */
    public static final String RESOURCE_STORE = "store";
    
    /** relative path. */
    public static final String RESOURCE_PROPERTYSTORE = "propertyStore";

    /** relative path. */
    public static final String RESOURCE_MONITORING = "monitoring";
    
    /** relative path for security. */
    public static final String RESOURCE_SECURITY = "security";
    
    /** relative path for cache. */
    public static final String RESOURCE_CACHE = "cache";
    
    /** relative path for cache. */
    public static final String STORE_CLEAR = "clear";
    
    /** relative path for cache. */
    public static final String STORE_CREATESCHEMA = "createSchema";

    /** relative path. */
    public static final String RESOURCE_FF4J = "ff4j";

    /** list of curves. */
    public static final String RESOURCE_PIE = "pieChart";
    
    /** list of curves. */
    public static final String RESOURCE_BAR = "barChart";
    
    /** filter for resource. */
    public static final String PARAM_START = "start";
    
    /** featureID. */
    public static final String PARAM_UID = "uid";
    
    /** filter for resource. */
    public static final String PARAM_END = "end";
    
    /** nb of points in the curve. */
    public static final String PARAM_NBPOINTS = "nbpoints";

    /** security role. */
    public static final String ROLE_READ = "READ";

    /** security role. */
    public static final String ROLE_WRITE = "WRITE";
    
    /** HTTP Parameter. */
    public static final String PARAM_AUTHKEY = "apiKey";
    
    /** HTTP Header. */
    public static final String HEADER_AUTHORIZATION = "Authorization";
    
    /** Manifest File. */
    public static final String MANIFEST_FILE = "/META-INF/MANIFEST.MF";

    /** Manifest File. */
    public static final String MANIFEST_VERSION = "Specification-Version";

    /** Hide default constructor. */
    private FF4jWebConstants() {}
}
