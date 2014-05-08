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

    /** attribute name. */
    String FF4J_SESSIONATTRIBUTE_NAME = "FF4J";
    
    /** ClassPath file which contains appendee of css. */
    String CSS_FILE = "ff4j-embedded.css";

    /** ClassPath file which contains appendee of js. */
    String JS_FILE = "ff4j-embedded.js";

    /** File encoding. */
    String UTF8_ENCODING = "UTF-8";

    /** Content type for response. */
    String CONTENT_TYPE_HTML = "text/html";

    /** Content type for response. */
    String CONTENT_TYPE_CSS = "text/css";

    /** Content type for response. */
    String CONTENT_TYPE_JS = "text/plain";

    /** User operation. */
    String OP_EDIT_FEATURE = "editfp";

    /** User operation: remove feature. */
    String OP_RMV_FEATURE = "rmvfp";

    /** User operation. */
    String OP_ADD_FEATURE = "addfp";

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

    /** User operation. */
    String OP_GETCSS = "getCSS";

    /** User operation. */
    String OP_GETJS = "getJS";

    /** HTTP Parameter. */
    String FEATID = "uid";

    /** HTTP Parameter. */
    String ROLE = "role";

    /** HTTP Parameter. */
    String DESCRIPTION = "desc";

    /** HTTP Parameter. */
    String OPERATION = "op";

    /** HTTP Parameter. */
    String FLIPFILE = "flipFile";

    /** Parametre. */
    String START_LINK = "<a href=\"";

    /** NewLine. */
    String NEW_LINE = System.getProperty("line.separator");

    /** Header. */
    String TEMPLATE_FILE = "ff4j-template.html";

    /** Table Header. */
    String TEMPLATE_TABLE_HEADER = "ff4j-template-table-header.html";

    /** Context Key. */
    String KEY_CTX = "CTX";
}
