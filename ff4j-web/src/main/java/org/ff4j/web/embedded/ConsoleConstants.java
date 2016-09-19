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
public class ConsoleConstants {
    
    // -------- CONTENT-TYPE ------------------------------

    /** Content type for response. */
    public static final String CONTENT_TYPE_HTML = "text/html";

    /** Content type for response. */
    public static final String CONTENT_TYPE_CSS = "text/css";

    /** Content type for response. */
    public static final String CONTENT_TYPE_JS = "application/javascript";
    
    /** Content type for response. */
    public static final String CONTENT_TYPE_JSON = "application/json";


    // -------- RESOURCES ------------------------------

    /** static resource param name. */
    public static final String RESOURCE = "rsc";

    /** static resource paramv alue. */
    public static final String RESOURCE_CSS_PARAM = "css";

    /** static resource file. */
    public static final String RESOURCE_CSS_FILE = "ff4j-embedded.css";

    /** static resource param value. */
    public static final String RESOURCE_JS_PARAM = "js";

    /** static resource file. */
    public static final String RESOURCE_JS_FILE = "ff4j-embedded.js";


    // -------- OPERATIONS ------------------------------

    /** POST - Operation. */
    public static final String OPERATION = "op";
    
    /** View for dispatch. */
    public static final String VIEW = "view";

    /** POST - Operation. */
    public static final String SUBOPERATION = "ope";

    /** User operation. */
    public static final String OP_CREATE_FEATURE = "create";
    
    /** User operation. */
    public static final String OP_CREATE_PROPERTY = "createProperty";

    /** User operation. */
    public static final String OP_EDIT_FEATURE = "update";
    
    /** User operation. */
    public static final String OP_EDIT_PROPERTY = "updateProperty";

    /** User operation: remove feature. */
    public static final String OP_RMV_FEATURE = "delete";
    
    /** User operation: remove feature. */
    public static final String OP_RMV_PROPERTY = "deleteProperty";
    
    /** User operation: remove feature. */
    public static final String OP_READ_PROPERTY = "readProperty";
    
    /** remove a value of a listed. */
    public static final String OP_DELETE_FIXEDVALUE = "deleteFixedValue";
    
    /** remove a value of a listed. */
    public static final String OP_ADD_FIXEDVALUE = "addFixedValue";

    /** User operation. */
    public static final String OP_TOGGLE_GROUP = "toggleGroup";
    
    /** User operation: remove feature. */
    public static final String OP_READ_FEATURE = "readFeature";

    /** User operation. */
    public static final String OP_ENABLE = "enable";

    /** User operation. */
    public static final String OP_DISABLE = "disable";

    /** User operation. */
    public static final String OP_IMPORT = "import";

    /** User operation. */
    public static final String OP_EXPORT = "export";
    
    /** User operation. */
    public static final String OP_MONITORING = "monitoring";


    // -------- TEMPLATING ------------------------------

    /** Header. */
    public static final String TEMPLATE_FILE = "ff4j-template.html";
    
    /** Monitoring. */
    public static final String TEMPLATE_FILE_MONITORING = "ff4j-monitoring.html";

    /** templating. */
    public static final String KEY_SERVLET_CONTEXT = "SERVLET_CONTEXT";

    /** templating. */
    public static final String KEY_VERSION = "VERSION";

    /** templating. */
    public static final String KEY_FEATURE_ROWS = "FEATURE_ROWS";
    
    /** templating. */
    public static final String KEY_PROPERTIES_ROWS = "PROPERTIES_ROWS";

    /** templating. */
    public static final String KEY_GROUP_LIST_EDIT = "FEATURE_GRPS_EDIT";

    /** templating. */
    public static final String KEY_GROUP_LIST_CREATE = "FEATURE_GRPS_CREATE";

    /** templating. */
    public static final String KEY_GROUP_LIST_TOGGLE = "FEATURE_GRPS_TOGGLE";

    /** templating alert. */
    public static final String KEY_ALERT_MESSAGE = "ALERT";
    
    /** templating alert. */
    public static final String KEY_AUDIT_ROWS = "AUDIT_ROWS";

    /** templating. */
    public static final String KEY_PERMISSIONLIST = "PERMISSIONS";


    // -------- FORM PARAM ------------------------------

    /** HTTP Parameter. */
    public static final String FEATID = "uid";

    /** HTTP Parameter. */
    public static final String ROLE = "role";

    /** HTTP Parameter. */
    public static final String DESCRIPTION = "desc";

    /** HTTP Parameter. */
    public static final String FLIPFILE = "flipFile";
    
    /** HTTP Parameter. */
    public static final String GROUPNAME = "groupName";

    /** HTTP Parameter. */
    public static final String STRATEGY = "strategy";
    
    /** HTTP Parameter. */
    public static final String STRATEGY_INIT = "initParams";

    /** HTTP Parameter. */
    public static final String PERMISSION = "permission";
    
    /** HTTP Parameter. */
    public static final String NAME = "name";

    // -------- MISC ------------------------------

    /** File encoding. */
    public static final String UTF8_ENCODING = "UTF-8";

    /** NewLine. */
    public static final String NEW_LINE = System.getProperty("line.separator");

    /** buffer size. */
    public static final int BUFFER_SIZE = 4096;

    /** servlet init param. */
    public static final String PROVIDER_PARAM_NAME = "ff4jProvider";

    /** attribute name. */
    public static final String FF4J_SESSIONATTRIBUTE_NAME = "FF4J";

    /** attribute name. */
    public static final String PREFIX_CHECKBOX = "perm-check-";

    /** permission. */
    public static final String PERMISSION_PUBLIC = "Public";

    /** permission. */
    public static final String PERMISSION_RESTRICTED = "Restricted";

    /** modal id. */
    public static final String MODAL_EDIT = "modalEdit";

    /** modal ID. */
    public static final String MODAL_CREATE = "modalCreate";

    /** modal ID. */
    public static final String MODAL_TOGGLE = "modalToggle";
    
    /** FixedValue to be remove. */
    public static final String PARAM_FIXEDVALUE = "fixedValue";

    private ConsoleConstants() {}

}