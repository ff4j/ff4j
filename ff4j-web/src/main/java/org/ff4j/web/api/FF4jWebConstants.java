package org.ff4j.web.api;

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
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public interface FF4jWebConstants {

    /** Header param sent on creation 201. */
    String LOCATION = "Location";

    /** expected post parameter from POST methods. */
    String POST_PARAMNAME_FEATURE_UID = "uid";

    /** expected post parameter from POST methods. */
    String POST_PARAMNAME_FLIPSTRATEGY = "flipstrategy";

    /** expected post parameter from POST methods. */
    String POST_PARAMNAME_ROLENAME = "rolename";

    /** expected post parameter from POST methods. */
    String POST_PARAMNAME_GROUPNAME = "groupName";

    /** parameter. */
    String POST_PARAMNAME_CUSTOM_PREFIX = "PARAM_";

    /** Custom operation on resource. */
    String OPERATION_ENABLE = "enable";

    /** Custom operation on resource. */
    String OPERATION_DISABLE = "disable";

    /** Custom operation on resource. */
    String OPERATION_FLIP = "flip";

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
}
