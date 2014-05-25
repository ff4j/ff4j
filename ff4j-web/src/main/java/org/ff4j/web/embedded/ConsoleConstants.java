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

    /** File encoding. */
    String UTF8_ENCODING = "UTF-8";

    /** Content type for response. */
    String CONTENT_TYPE_HTML = "text/html";

    /** Content type for response. */
    String CONTENT_TYPE_CSS = "text/css";

    /** Content type for response. */
    String CONTENT_TYPE_JS = "text/plain";

    /** attribute name. */
    String FF4J_SESSIONATTRIBUTE_NAME = "FF4J";

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


    /** POST - Operation. */
    String OPERATION = "op";

    /** User operation. */
    String OP_CREATE_FEATURE = "create";

    /** User operation. */
    String OP_EDIT_FEATURE = "editfp";

    /** User operation: remove feature. */
    String OP_RMV_FEATURE = "rmvfp";

    /** User operation. */
    String OP_ENABLE = "enable";

    /** User operation. */
    String OP_DISABLE = "disable";

    /** User operation. */
    String OP_ADD_ROLE = "addrole";

    /** User operation. */
    String OP_RMV_ROLE = "rmvrole";

    /** User operation. */
    String OP_IMPORT = "import";

    /** User operation. */
    String OP_EXPORT = "export";

    /** templating. */
    String KEY_SERVLET_CONTEXT = "SERVLET_CTX";

    /** templating. */
    String KEY_FEATURE_ROWS = "FEATURE_ROWS";

    /** templating. */
    String KEY_GROUP_LIST = "GROUP_LIST";

    /** templating alert. */
    String KEY_ALERT_MESSAGE = "ALERT";

    /** HTTP Parameter. */
    String FEATID = "uid";

    /** HTTP Parameter. */
    String ROLE = "role";

    /** HTTP Parameter. */
    String DESCRIPTION = "desc";

    /** HTTP Parameter. */
    String FLIPFILE = "flipFile";

    /** NewLine. */
    String NEW_LINE = System.getProperty("line.separator");

    /** Header. */
    String TEMPLATE_FILE = "ff4j-template.html";


}
