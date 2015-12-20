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
public interface FF4jWebConstants {

    /** Header param sent on creation 201. */
    String LOCATION = "Location";

    /** expected post parameter from POST methods. */
    String POST_PARAMNAME_FEATURE_UID = "uid";
    
    /** parameter. */
    String POST_PARAMNAME_CUSTOM_PREFIX = "PARAM_";

    /** Custom operation on resource. */
    String OPERATION_ENABLE = "enable";

    /** Custom operation on resource. */
    String OPERATION_DISABLE = "disable";

    /** Custom operation on resource. */
    String OPERATION_CHECK = "check";

    /** Custom operation on resource. */
    String OPERATION_GRANTROLE = "grantrole";

    /** Custom operation on resource. */
    String OPERATION_REMOVEROLE = "removerole";

    /** Custom operation on resource. */
    String OPERATION_ADDGROUP = "addGroup";

    /** Custom operation on resource. */
    String OPERATION_REMOVEGROUP = "removeGroup";

    /** relative path. */
    String RESOURCE_FEATURES = "features";

    /** relative path. */
    String RESOURCE_GROUPS = "groups";

    /** relative path. */
    String RESOURCE_STORE = "store";

    /** relative path. */
    String RESOURCE_MONITORING = "monitoring";
    
    /** relative path for security. */
    String RESOURCE_SECURITY = "security";
    
    /** relative path for cache. */
    String RESOURCE_CACHE = "cache";

    /** relative path. */
    String RESOURCE_FF4J = "ff4j";

    /** list of curves. */
    String RESOURCE_PIE = "pieChart";
    
    /** list of curves. */
    String RESOURCE_BAR = "barChart";
    
    /** filter for resource. */
    String PARAM_START = "start";
    
    /** featureID. */
    String PARAM_UID = "uid";
    
    /** filter for resource. */
    String PARAM_END = "end";
    
    /** nb of points in the curve. */
    String PARAM_NBPOINTS = "nbpoints";

    /** security role. */
    String ROLE_READ = "READ";

    /** security role. */
    String ROLE_WRITE = "WRITE";
    
    /** HTTP Parameter. */
    String PARAM_AUTHKEY = "apiKey";
    
    /** HTTP Header. */
    String HEADER_AUTHORIZATION = "Authorization";
    
    /** Manifest File. */
    String MANIFEST_FILE = "/META-INF/MANIFEST.MF";
    
    String MANIFEST_VERSION = "Specification-Version";
}
