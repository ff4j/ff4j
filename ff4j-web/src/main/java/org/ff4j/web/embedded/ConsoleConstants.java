package org.ff4j.web.embedded;

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
 * Global constants to work with web console ff4j.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public interface ConsoleConstants {

    
    // -------- CONTENT-TYPE ------------------------------

    /** Content type for response. */
    String CONTENT_TYPE_HTML = "text/html";

    /** Content type for response. */
    String CONTENT_TYPE_CSS = "text/css";

    /** Content type for response. */
    String CONTENT_TYPE_JS = "application/javascript";


    // -------- RESOURCES ------------------------------

    /** static resource param name. */
    String RESOURCE = "rsc";

    /** static resource paramv alue. */
    String RESOURCE_CSS_PARAM = "css";

    /** static resource file. */
    String RESOURCE_CSS_FILE = "ff4j-embedded.css";

    /** static resource param value. */
    String RESOURCE_JS_PARAM = "js";

    /** static resource file. */
    String RESOURCE_JS_FILE = "ff4j-embedded.js";


    // -------- OPERATIONS ------------------------------

    /** POST - Operation. */
    String OPERATION = "op";

    /** POST - Operation. */
    String SUBOPERATION = "ope";

    /** User operation. */
    String OP_CREATE_FEATURE = "create";

    /** User operation. */
    String OP_EDIT_FEATURE = "update";

    /** User operation: remove feature. */
    String OP_RMV_FEATURE = "delete";

    /** User operation. */
    String OP_TOGGLE_GROUP = "toggleGroup";

    /** User operation. */
    String OP_ENABLE = "enable";

    /** User operation. */
    String OP_DISABLE = "disable";

    /** User operation. */
    String OP_IMPORT = "import";

    /** User operation. */
    String OP_EXPORT = "export";


    // -------- TEMPLATING ------------------------------

    /** Header. */
    String TEMPLATE_FILE = "ff4j-template.html";

    /** templating. */
    String KEY_SERVLET_CONTEXT = "SERVLET_CONTEXT";

    /** templating. */
    String KEY_VERSION = "VERSION";

    /** templating. */
    String KEY_FEATURE_ROWS = "FEATURE_ROWS";

    /** templating. */
    String KEY_GROUP_LIST_EDIT = "FEATURE_GRPS_EDIT";

    /** templating. */
    String KEY_GROUP_LIST_CREATE = "FEATURE_GRPS_CREATE";

    /** templating. */
    String KEY_GROUP_LIST_TOGGLE = "FEATURE_GRPS_TOGGLE";

    /** templating alert. */
    String KEY_ALERT_MESSAGE = "ALERT";

    /** templating. */
    String KEY_PERMISSIONLIST = "PERMISSIONS";


    // -------- FORM PARAM ------------------------------

    /** HTTP Parameter. */
    String FEATID = "uid";

    /** HTTP Parameter. */
    String ROLE = "role";

    /** HTTP Parameter. */
    String DESCRIPTION = "desc";

    /** HTTP Parameter. */
    String FLIPFILE = "flipFile";
    
    /** HTTP Parameter. */
    String GROUPNAME = "groupName";

    /** HTTP Parameter. */
    String STRATEGY = "strategy";
    
    /** HTTP Parameter. */
    String STRATEGY_INIT = "initParams";

    /** HTTP Parameter. */
    String PERMISSION = "permission";

    // -------- MISC ------------------------------

    /** File encoding. */
    String UTF8_ENCODING = "UTF-8";

    /** NewLine. */
    String NEW_LINE = System.getProperty("line.separator");

    /** buffer size. */
    int BUFFER_SIZE = 4096;

    /** servlet init param. */
    String PROVIDER_PARAM_NAME = "ff4jProvider";

    /** attribute name. */
    String FF4J_SESSIONATTRIBUTE_NAME = "FF4J";

    /** attribute name. */
    String PREFIX_CHECKBOX = "perm-check-";

    /** permission. */
    String PERMISSION_PUBLIC = "Public";

    /** permission. */
    String PERMISSION_RESTRICTED = "Restricted";

    /** modal id. */
    String MODAL_EDIT = "modalEdit";

    /** modal ID. */
    String MODAL_CREATE = "modalCreate";

    /** modal ID. */
    String MODAL_TOGGLE = "modalToggle";

}
