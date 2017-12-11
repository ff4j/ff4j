package org.ff4j.web.bean;

/*
 * #%L
 * ff4j-sample-web
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
 * All constants
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class WebConstants {
	
	// -------- HTTP STATUS ------------------------------
    
	/** http status. */
    public static final int STATUS_BADREQUEST = 400;
    
	/** http status. */
	public static final int STATUS_FORBIDDEN = 403;
	
	/** http status. */
    public static final int STATUS_NOT_FOUND = 404;
    
	
	// -------- VIEWS ------------------------------
	
	/** View for dispatch. */
    public static final String VIEW = "view";
    
	/** constant. */
	public static final String VIEW_404 = "404";
	
	/** constant. */
	public static final String VIEW_DEFAULT = "home";

	/** constant. */
	public static final String VIEW_STATIC = "static";

	/** constant. */
	public static final String VIEW_API = "api";
	
	/** View name. */
    public static final String VIEW_FEATURE_USAGE = "featureUsage";
    
    /** View name. */
    public static final String VIEW_TIME_SERIES = "timeSeries";
    
    /** View name. */
    public static final String VIEW_AUDIT = "audit";
		
	// -------- CONTENT-TYPE ------------------------------

    /** Content type for response. */
    public static final String CONTENT_TYPE_HTML = "text/html";

    /** Content type for response. */
    public static final String CONTENT_TYPE_TEXT = "text/plain";

    /** Content type for response. */
    public static final String CONTENT_TYPE_CSS = "text/css";

    /** Content type for response. */
    public static final String CONTENT_TYPE_JS = "application/javascript";

    /** Content type for response. */
    public static final String CONTENT_TYPE_JSON = "application/json";

    /** Content type for response. */
    public static final String CONTENT_TYPE_FONT = "font/opentype";


    // -------- RESOURCES ------------------------------

    /** static resource param name. */
    public static final String RESOURCE = "rsc";

    /** static resource paramv alue. */
    public static final String RESOURCE_CSS_PARAM = "css";

    /** static resource file. */
    public static final String RESOURCE_CSS_FILE = "ff4j-embedded-css.dat";

    /** static resource param value. */
    public static final String RESOURCE_JS_PARAM = "js";

    /** static resource file. */
    public static final String RESOURCE_JS_FILE = "ff4j-embedded-js.dat";

    // -------- OPERATIONS ------------------------------

    /** User operation. */
    public static final String OP_IMPORT = "import";

    /** User operation. */
    public static final String OP_EXPORT = "export";
    
    /** User operation: remove feature. */
    public static final String OP_READ_FEATURE = "readFeature";
    
    /** User operation. */
    public static final String OP_CREATE_FEATURE = "create";

    /** User operation. */
    public static final String OP_CREATE_PROPERTY = "createProperty";
    
    /** User operation. */
    public static final String OP_CREATE_SCHEMA = "createSchema";

    /** User operation. */
    public static final String OP_EDIT_FEATURE = "update";

    /** User operation. */
    public static final String OP_EDIT_PROPERTY = "updateProperty";

    /** User operation: remove feature. */
    public static final String OP_RMV_FEATURE = "delete";
    
    /** User operation: remove feature. */
    public static final String OP_ADD_PERMISSION = "addPermission";
    
    /** User operation: remove feature. */
    public static final String OP_RMV_PERMISSION = "deletePermission";
    
    /** User operation: remove feature. */
    public static final String OP_CLEAR_PERMISSIONS = "clearPermissions";

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
    
    /** User operation. */
    public static final String OP_RENAME_FEATURE = "renameFeature";
    
    /** User operation. */
    public static final String OP_COPY_FEATURE = "copyFeature";
    
    /** User operation. */
    public static final String OP_RENAME_PROPERTY = "renameProperty";
    
    /** User operation. */
    public static final String OP_COPY_PROPERTY = "copyProperty";
    
    /** User operation: remove feature. */
    public static final String OP_FEATURES = "features";

    /** User operation: remove feature. */
    public static final String OP_PROPERTIES = "properties";

    /** User operation. */
    public static final String OP_ENABLE = "enable";

    /** User operation. */
    public static final String OP_DISABLE = "disable";
    
    /** User operation. */
    public static final String TOGGLE_AUDIT = "toggleAudit";

    /** User operation. */
    public static final String OP_MONITORING = "monitoring";
    
    /** User operation. */
    public static final String OP_FEATUREUSAGE = "featureUsage";
    
    /** User operation. */
    public static final String OP_TIMESERIES = "timeSeries";
    
    /** User operation. */
    public static final String OP_AUDIT = "audit";

    /** User operation. */
    public static final String OP_CLEAR_CACHE = "clearCache";
    
    // -------- GraphNames ------------------------------

    /** hit ratio. */
    public static final String GRAPH_PIE_HITRATIO = "pieHitRatio";
    
    /** hit ratio. */
    public static final String GRAPH_BAR_HITRATIO = "barHitRatio";
    
    /** hit ratio. */
    public static final String GRAPH_PIE_USER = "pieUserRatio";
    
    /** hit ratio. */
    public static final String GRAPH_PIE_HOST = "pieHostRatio";
    
    /** hit ratio. */
    public static final String GRAPH_PIE_SOURCE = "pieSourceRatio";
    
    // -------- TEMPLATING ------------------------------

    /** Header. */
    public static final String TEMPLATE_FILE = "ff4j-template.dat";

    /** Monitoring. */
    public static final String TEMPLATE_FILE_MONITORING = "ff4j-monitoring.dat";

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

    /** custom css if defined in web.xml. */
    public static final String KEY_CSS_URL = "URL_CSS";
    
    /** context name in audit bean. */
    public static final String KEY_AUDITTRAIL = "auditTrail";

    /** context name in audit bean. */
    public static final String KEY_AUDITENABLE = "auditEnable";

    
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
    
    /** HTTP Parameter. */
    public static final String FEATURE_UID = "featureid";
    
    /** HTTP Parameter. */
    public static final String NEW_NAME = "newname";
    
    /** HTTP Parameter. */
    public static final String START_DATE = "sd";
    
    /** HTTP Parameter. */
    public static final String END_DATE = "ed";
    
    /** HTTP Parameter. */
    public static final String KEY_DATE = "key";
    
    // -------- MISC ------------------------------
    
    /** POST - Operation. */
    public static final String OPERATION = "op";  

    /** POST - Operation. */
    public static final String SUBOPERATION = "ope";
    
    /** POST - Langue. */
    public static final String LANG = "lang";
    
    /** POST - Langue. */
    public static final String LANG_ATTRIBUTE = "ff4j_console_language";
    
    /** constant. */
    public static final String ERROR = "error";

    /** File encoding. */
    public static final String UTF8_ENCODING = "UTF-8";

    /** NewLine. */
    public static final String NEW_LINE = System.getProperty("line.separator");

    /** buffer size. */
    public static final int BUFFER_SIZE = 4096;

    /** servlet init param. */
    public static final String SERVLETPARAM_FF4JPROVIDER = "ff4jProvider";

    /** servlet init param. */
    public static final String SERVLETPARAM_CSS = "customCSS";

    /** attribute name. */
    public static final String FF4J_SESSIONATTRIBUTE_NAME = "FF4J";

    /** attribute name. */
    public static final String CSS_SESSIONATTRIBUTE_NAME = "customCSS";

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

    
    // -------- ROLES ------------------------------
    
    /** Read features state. */
    public static final String ROLE_USER = "FF4J_CONSOLE_READ";
    
    /** Edit and create state. */
    public static final String ROLE_MANAGER = "FF4J_CONSOLE_WRITE";
    
    /** Administration (audit, cache...). */
    public static final String ROLE_ADMIN = "FF4J_CONSOLE_ADMIN";
    
    // -------- PARAMS -------------------------------
    
    /** FixedValue to be remove. */
    public static final String PARAM_FIXEDVALUE = "fixedValue";
    
    /** Picture name for empty field. */
    public static final String PIC_DISABLE = "disabled";
    
    /**
     * Hide defaulf constructor.
     */
    private WebConstants() {
    }

  
}
