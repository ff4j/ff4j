package org.ff4j.web.embedded;

import static org.ff4j.web.embedded.ConsoleConstants.CONTENT_TYPE_CSS;
import static org.ff4j.web.embedded.ConsoleConstants.CONTENT_TYPE_HTML;
import static org.ff4j.web.embedded.ConsoleConstants.CONTENT_TYPE_JS;
import static org.ff4j.web.embedded.ConsoleConstants.FEATID;
import static org.ff4j.web.embedded.ConsoleConstants.KEY_ALERT_MESSAGE;
import static org.ff4j.web.embedded.ConsoleConstants.KEY_AUDIT_ROWS;
import static org.ff4j.web.embedded.ConsoleConstants.KEY_FEATURE_ROWS;
import static org.ff4j.web.embedded.ConsoleConstants.KEY_GROUP_LIST_CREATE;
import static org.ff4j.web.embedded.ConsoleConstants.KEY_GROUP_LIST_EDIT;
import static org.ff4j.web.embedded.ConsoleConstants.KEY_GROUP_LIST_TOGGLE;
import static org.ff4j.web.embedded.ConsoleConstants.KEY_PERMISSIONLIST;
import static org.ff4j.web.embedded.ConsoleConstants.KEY_PROPERTIES_ROWS;
import static org.ff4j.web.embedded.ConsoleConstants.KEY_SERVLET_CONTEXT;
import static org.ff4j.web.embedded.ConsoleConstants.KEY_VERSION;
import static org.ff4j.web.embedded.ConsoleConstants.MODAL_CREATE;
import static org.ff4j.web.embedded.ConsoleConstants.MODAL_EDIT;
import static org.ff4j.web.embedded.ConsoleConstants.MODAL_TOGGLE;
import static org.ff4j.web.embedded.ConsoleConstants.NEW_LINE;
import static org.ff4j.web.embedded.ConsoleConstants.OP_RMV_FEATURE;
import static org.ff4j.web.embedded.ConsoleConstants.OP_RMV_PROPERTY;
import static org.ff4j.web.embedded.ConsoleConstants.PREFIX_CHECKBOX;
import static org.ff4j.web.embedded.ConsoleConstants.RESOURCE;
import static org.ff4j.web.embedded.ConsoleConstants.RESOURCE_CSS_FILE;
import static org.ff4j.web.embedded.ConsoleConstants.RESOURCE_CSS_PARAM;
import static org.ff4j.web.embedded.ConsoleConstants.RESOURCE_JS_FILE;
import static org.ff4j.web.embedded.ConsoleConstants.RESOURCE_JS_PARAM;
import static org.ff4j.web.embedded.ConsoleConstants.TEMPLATE_FILE;
import static org.ff4j.web.embedded.ConsoleConstants.TEMPLATE_FILE_MONITORING;
import static org.ff4j.web.embedded.ConsoleConstants.UTF8_ENCODING;

import java.io.IOException;

/*
 * #%L AdministrationConsoleRenderer.java (ff4j-web) by Cedrick LUNVEN %% Copyright (C) 2013 Ff4J %% Licensed under the Apache
 * License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of
 * the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import java.io.InputStream;
import java.io.PrintWriter;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ff4j.FF4j;
import org.ff4j.audit.Event;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.repository.EventRepository;
import org.ff4j.core.Feature;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyBigDecimal;
import org.ff4j.property.PropertyBigInteger;
import org.ff4j.property.PropertyBoolean;
import org.ff4j.property.PropertyByte;
import org.ff4j.property.PropertyDouble;
import org.ff4j.property.PropertyFloat;
import org.ff4j.property.PropertyInt;
import org.ff4j.property.PropertyLogLevel;
import org.ff4j.property.PropertyLong;
import org.ff4j.property.PropertyShort;
import org.ff4j.property.PropertyString;
import org.ff4j.utils.Util;

/**
 * Used to build GUI Interface for feature flip servlet. It contains gui component render and parmeters
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public final class ConsoleRenderer {

    /** Cache for page blocks. */
    private static String htmlTemplate = null;
    
    /** Cache for page blocks. */
    private static String htmlTemplateMonitoring = null;

    /** Load CSS. */
    private static String cssContent = null;

    /** Load JS. */
    private static String jsContent = null;

    /** Cache for page blocks. */
    static final String TABLE_FEATURES_FOOTER = "" + "</tbody></table></form></fieldset>";

    /** fin de ligne. **/
    static final String END_OF_LINE = "\r\n";

    /** Get version of the component. */
    static final String FF4J_VERSION = ConsoleRenderer.class.getPackage().getImplementationVersion();
    
    /** Display audit log date. */
    static final SimpleDateFormat SDF = new SimpleDateFormat("yyyy-dd-MM HH:mm:ss"); 
    
    /** Mapping from simple 'String' <=> 'org.ff4j.property.PropertyString'. */
    private static Map < String , String > uxTypes = new HashMap< String , String>();
    
    /**
     * Initialized Primitive to work with Properties.
     */
    static {
        uxTypes.put(Byte.class.getSimpleName(), PropertyByte.class.getName());
        uxTypes.put(Short.class.getSimpleName(), PropertyShort.class.getName());
        uxTypes.put(Integer.class.getSimpleName(), PropertyInt.class.getName());
        uxTypes.put(Long.class.getSimpleName(), PropertyLong.class.getName());
        uxTypes.put(Double.class.getSimpleName(), PropertyDouble.class.getName());
        uxTypes.put(Boolean.class.getSimpleName(), PropertyBoolean.class.getName());
        uxTypes.put(Float.class.getSimpleName(), PropertyFloat.class.getName());
        uxTypes.put(BigInteger.class.getSimpleName(), PropertyBigInteger.class.getName());
        uxTypes.put(BigDecimal.class.getSimpleName(), PropertyBigDecimal.class.getName());
        uxTypes.put("LogLevel", PropertyLogLevel.class.getName());
        uxTypes.put(String.class.getSimpleName(), PropertyString.class.getName());
    }
    
    private ConsoleRenderer() {}
    
    /**
     * Render the ff4f console webpage through different block.
     * 
     * @param req
     *            http request (with parameters)
     * @param res
     *            http response (with outouput test)
     * @param message
     *            text in the information box (blue/green/orange/red)
     * @param messagetype
     *            type of informatice message (info,success,warning,error)
     * @throws IOException
     *             error during populating http response
     */
    public static void renderPage(FF4j ff4j, HttpServletRequest req, HttpServletResponse res, String msg, String msgType) throws IOException {
        res.setContentType(CONTENT_TYPE_HTML);
        PrintWriter out = res.getWriter();

        // Header of the page
        String htmlContent = renderTemplate(req);

        // Subsctitution MESSAGE BOX
        htmlContent = htmlContent.replaceAll("\\{" + KEY_ALERT_MESSAGE + "\\}", renderMessageBox(msg, msgType));

        // Subsctitution FEATURE_ROWS
        try {
            htmlContent = htmlContent.replaceAll("\\{" + KEY_FEATURE_ROWS + "\\}", renderFeatureRows(ff4j, req));
        } catch(IllegalArgumentException ieo) {
            htmlContent = htmlContent.replaceAll("\\{" + KEY_FEATURE_ROWS + "\\}", 
                    "Cannot render Features please check names (no $) '" + ieo.getMessage() + "'");
        }
        // substitution PROPERTIES_ROWS
        try {
            htmlContent = htmlContent.replaceAll("\\{" + KEY_PROPERTIES_ROWS + "\\}", renderPropertiesRows(ff4j, req));
        } catch(IllegalArgumentException ieo) {
            htmlContent = htmlContent.replaceAll("\\{" + KEY_PROPERTIES_ROWS + "\\}", 
                    "Cannot render propertie please check names (no $) '" + ieo.getMessage() + "'");
        }
        
        // Substitution GROUP_LIST
        String groups = ConsoleRenderer.renderGroupList(ff4j, MODAL_EDIT);
        htmlContent = htmlContent.replaceAll("\\{" + KEY_GROUP_LIST_EDIT + "\\}", groups);
        groups = groups.replaceAll(MODAL_EDIT, MODAL_CREATE);
        htmlContent = htmlContent.replaceAll("\\{" + KEY_GROUP_LIST_CREATE + "\\}", groups);
        groups = groups.replaceAll(MODAL_CREATE, MODAL_TOGGLE);
        htmlContent = htmlContent.replaceAll("\\{" + KEY_GROUP_LIST_TOGGLE + "\\}", groups);

        // Substitution PERMISSIONS
        final String permissions = renderPermissionList(ff4j);
        htmlContent = htmlContent.replaceAll("\\{" + KEY_PERMISSIONLIST + "\\}", permissions);

        out.println(htmlContent);
    }
    
    
    /**
     * Render the ff4f console webpage through different block.
     * 
     * @param req
     *            http request (with parameters)
     * @param res
     *            http response (with outouput test)
     * @param msg
     *            text in the information box (blue/green/orange/red)
     * @param msgType
     *            type of informatice message (info,success,warning,error)
     * @throws IOException
     *             error during populating http response
     */
    public static void renderPageMonitoring(FF4j ff4j, HttpServletRequest req, HttpServletResponse res, String msg, String msgType) throws IOException {
        res.setContentType(CONTENT_TYPE_HTML);
        PrintWriter out = res.getWriter();
        String htmlContent = renderTemplateMonitoring(req);
        htmlContent = htmlContent.replaceAll("\\{" + KEY_ALERT_MESSAGE + "\\}", renderMessageBox(msg, msgType));
        htmlContent = htmlContent.replaceAll("\\{" + KEY_AUDIT_ROWS + "\\}", renderAuditRows(ff4j , req));
        out.println(htmlContent);
    }

    /**
     * Build info messages.
     * 
     * @param featureName
     *            target feature name
     * @param operationId
     *            target operationId
     * @return
     */
    public static String msg(String featureName, String operationId) {
        return String.format("Feature <b>%s</b> has been successfully %s", featureName, operationId);
    }
    
    /**
     * Build info messages.
     * 
     * @param featureName
     *            target feature name
     * @param operationId
     *            target operationId
     * @return
     */
    public static  String renderMsgProperty(String featureName, String operationId) {
        return String.format("Property <b>%s</b> has been successfully %s", featureName, operationId);
    }

    /**
     * Build info messages.
     * 
     * @param groupName
     *            target group name
     * @param operationId
     *            target operationId
     * @return
     */
    public static String renderMsgGroup(String groupName, String operationId) {
        return String.format("Group <b>%s</b> has been successfully %s", groupName, operationId);
    }
    
    /**
     * Deliver CSS and Javascript files/
     * 
     * @param req
     *            request
     * @param res
     *            response
     * @return value for resources
     * @throws IOException
     *             exceptions
     */
    public static boolean renderResources(HttpServletRequest req, HttpServletResponse res) throws IOException {
        // Serve static resource file as CSS and Javascript
        String resources = req.getParameter(RESOURCE);
        if (resources != null && !resources.isEmpty()) {
            if (RESOURCE_CSS_PARAM.equalsIgnoreCase(resources)) {
                res.setContentType(CONTENT_TYPE_CSS);
                res.getWriter().println(ConsoleRenderer.getCSS());
                return true;
            } else if (RESOURCE_JS_PARAM.equalsIgnoreCase(resources)) {
                res.setContentType(CONTENT_TYPE_JS);
                res.getWriter().println(ConsoleRenderer.getJS());
                return true;
            }
        }
        return false;
    }
    
    /**
     * Display message box if message.
     * 
     * @param message
     *            target message to display
     * @param type
     *            type of messages
     * @return html content to be displayed as message
     */
    public static String renderMessageBox(String message, String type) {
        StringBuilder sb = new StringBuilder();
        // Display Message box
        if (message != null && !message.isEmpty()) {
            sb.append("<div class=\"alert alert-" + type + "\" >");
            sb.append("<button type=\"button\" class=\"close\" data-dismiss=\"alert\">&times;</button>");
            sb.append("<span style=\"font-style:normal;color:#696969;\">");
            sb.append(message);
            sb.append("</span>");
            sb.append("</div>");
        }
        return sb.toString();
    }
    
    /**
     * Load HTML template file and substitute by current URL context path
     * 
     * @param req
     *            current http request
     * @return current text part as string
     */
     private static final String renderTemplate(HttpServletRequest req) {
        if (htmlTemplate == null || htmlTemplate.isEmpty()) {
            String ctx = req.getContextPath() + req.getServletPath() + "";
            htmlTemplate = loadFileAsString(TEMPLATE_FILE);
            htmlTemplate = htmlTemplate.replaceAll("\\{" + KEY_SERVLET_CONTEXT + "\\}", ctx);
            htmlTemplate = htmlTemplate.replaceAll("\\{" + KEY_VERSION + "\\}", FF4J_VERSION);
        }
        return htmlTemplate;
    }
     
     /**
      * Load HTML template file and substitute by current URL context path
      * 
      * @param req
      *            current http request
      * @return current text part as string
      */
      private static final String renderTemplateMonitoring(HttpServletRequest req) {
         if (htmlTemplateMonitoring == null || htmlTemplateMonitoring.isEmpty()) {
             String ctx = req.getContextPath() + req.getServletPath() + "";
             htmlTemplateMonitoring = loadFileAsString(TEMPLATE_FILE_MONITORING);
             htmlTemplateMonitoring = htmlTemplateMonitoring.replaceAll("\\{" + KEY_SERVLET_CONTEXT + "\\}", ctx);
             htmlTemplateMonitoring = htmlTemplateMonitoring.replaceAll("\\{" + KEY_VERSION + "\\}", FF4J_VERSION);
         }
         return htmlTemplateMonitoring;
     }
      
    public static String renderValue(String source, int column) {
        StringBuilder sb = new StringBuilder();
        source = source.replaceAll("\\\\", "/");
        source = source.replaceAll("\\$", "&dollar;");
        while (source.length() > column) {
            sb.append(source.substring(0,  column));
            sb.append("\r\n<br>");
            source = source.substring(column);
        }
        sb.append(source);
        return sb.toString();
    }
    
    private static final String renderPropertiesRows(FF4j ff4j, HttpServletRequest req) {
        StringBuilder sb = new StringBuilder();
        final Map < String, Property<?>> mapOfProperties = ff4j.getProperties();
        for(Map.Entry<String,Property<?>> uid : mapOfProperties.entrySet()) {
            Property<?> currentProperty = uid.getValue();
            sb.append("<tr>" + END_OF_LINE);
            
            // Column with uid and description as tooltip
            sb.append("<td><a class=\"ff4j-properties\" ");
            if (null != currentProperty.getDescription()) {
                sb.append(" tooltip=\"");
                sb.append(currentProperty.getDescription());
                sb.append("\"");
            }
            sb.append(">");
            sb.append(renderValue(currentProperty.getName(), 50));
            sb.append("</a>");
           
            // Colonne Value
            sb.append("</td><td>");
            if (null != currentProperty.asString()) {
                sb.append(renderValue(currentProperty.asString(), 60));
            } else {
                sb.append("--");
            }    
            
            // Colonne Type
            sb.append("</td><td>");
            if (uxTypes.containsValue(currentProperty.getType())) {
                sb.append(Util.getFirstKeyByValue(uxTypes, currentProperty.getType()));
            } else {
                sb.append(currentProperty.getType());
            }
            
            // Colonne Fixed Value
            sb.append("</td><td>");
            if (null != currentProperty.getFixedValues()) {
                for (Object o : currentProperty.getFixedValues()) {
                    sb.append("<li>" + o.toString());
                }
            } else {
                sb.append("--");
            }
            
            // Colonne Button Edit
            sb.append("</td><td style=\"width:5%;text-align:center\">");
            sb.append("<a data-toggle=\"modal\" href=\"#modalEditProperty\" data-pname=\"" + currentProperty.getName() + "\" ");
            sb.append(" style=\"width:6px;\" class=\"open-EditPropertyDialog btn\">");
            sb.append("<i class=\"icon-pencil\" style=\"margin-left:-5px;\"></i></a>");

            // Colonne Button Delete
            sb.append("</td><td style=\"width:5%;text-align:center\">");
            sb.append("<a href=\"");
            sb.append(req.getContextPath());
            sb.append(req.getServletPath());
            sb.append("?op=" + OP_RMV_PROPERTY + "&" + FEATID + "=" + currentProperty.getName());
            sb.append("\" style=\"width:6px;\" class=\"btn\">");
            sb.append("<i class=\"icon-trash\" style=\"margin-left:-5px;\"></i>");
            sb.append("</a>");
            sb.append("</td></tr>");
        }
        return sb.toString();
    }

    private static final String renderAuditRows(FF4j ff4j, HttpServletRequest req) {
    	StringBuilder sb = new StringBuilder();
    	EventRepository er = ff4j.getEventRepository();
    	EventQueryDefinition query = new EventQueryDefinition();
    	for (Event event : er.searchFeatureUsageEvents(query)) {
    		sb.append("<tr>" + END_OF_LINE);
        	sb.append("<td>" + SDF.format(new Date(event.getTimestamp())) + "</td>");
        	sb.append("<td>" + event.getType() + "</td>");
        	sb.append("<td>" + event.getName() + "</td>");
        	sb.append("<td>" + event.getAction() + "</td>");
        	sb.append("</tr>");
		}
    	return sb.toString();
    }
    
    /**
     * Produce the rows of the Feature Table.
     *
     * @param ff4j
     *            target ff4j.
     * @param req
     *            current http request
     * @return string representing the list of features
     */
    private static final String renderFeatureRows(FF4j ff4j, HttpServletRequest req) {
        StringBuilder sb = new StringBuilder();
        final Map < String, Feature> mapOfFeatures = ff4j.getFeatures();
        for(Map.Entry<String,Feature> uid : mapOfFeatures.entrySet()) {
            Feature currentFeature = uid.getValue();
            sb.append("<tr>" + END_OF_LINE);
            
            // Column with uid and description as tooltip
            sb.append("<td><a class=\"ff4j-tooltip\" ");
            if (null != currentFeature.getDescription()) {
                sb.append(" tooltip=\"");
                sb.append(currentFeature.getDescription());
                sb.append("\"");
            }
            sb.append(">");
            sb.append(currentFeature.getUid());
            sb.append("</a>");
            
            // Colonne Group
            sb.append("</td><td>");
            if (null != currentFeature.getGroup()) {
                sb.append(currentFeature.getGroup());
            } else {
                sb.append("--");
            }
            
            // Colonne Permissions
            sb.append("</td><td>");
            Set < String > permissions = currentFeature.getPermissions();
            if (null != permissions && !permissions.isEmpty()) {
                boolean first = true;
                for (String perm : permissions) {
                    if (!first) {
                        sb.append(",");
                    }
                    sb.append(perm);
                    first = false;
                }
            } else {
                sb.append("--");
            }
            
            // Colonne Strategy
            sb.append("</td><td style=\"word-break: break-all;\">");
            FlippingStrategy fs = currentFeature.getFlippingStrategy();
            if (null != fs) {
                sb.append(renderValue(fs.getClass().getName(), 50));
                if (fs.getInitParams() != null) {
                    for (Map.Entry<String, String> entry : fs.getInitParams().entrySet()) {
                        sb.append("<li>" + renderValue(entry.getKey() + " =  " + entry.getValue(), 40));     
                    } 
                }
            } else {
                sb.append("--");
            }
            
            // Colonne 'Holy' Toggle
            sb.append("</td><td style=\"width:8%;text-align:center\">");
            sb.append("<label class=\"switch switch-green\">");
            sb.append("<input id=\"" + currentFeature.getUid() + "\" type=\"checkbox\" class=\"switch-input\"");
            sb.append(" onclick=\"javascript:toggle(this)\" ");
            if (currentFeature.isEnable()) {
                sb.append(" checked");
            }
            sb.append(">");
            sb.append("<span class=\"switch-label\" data-on=\"On\" data-off=\"Off\"></span>");
            sb.append("<span class=\"switch-handle\"></span>");
            sb.append("</label>");
            
            // Colonne Button Edit
            sb.append("</td><td style=\"width:5%;text-align:center\">");
            sb.append("<a data-toggle=\"modal\" href=\"#modalEdit\" data-id=\"" + currentFeature.getUid() + "\" ");
            sb.append(" data-desc=\"" + currentFeature.getDescription() + "\"");
            sb.append(" data-group=\"" + currentFeature.getGroup() + "\"");
            sb.append(" data-strategy=\"");
            if (null != currentFeature.getFlippingStrategy()) {
                sb.append(currentFeature.getFlippingStrategy().getClass().getName());
            }
            sb.append("\" data-stratparams=\"");
            if (null != currentFeature.getFlippingStrategy()) {
                sb.append(currentFeature.getFlippingStrategy().getInitParams());
            }
            sb.append("\" data-permissions=\"");
            if (null != currentFeature.getPermissions() && !currentFeature.getPermissions().isEmpty()) {
                sb.append(currentFeature.getPermissions());
            }
            sb.append("\" style=\"width:6px;\" class=\"open-EditFlipDialog btn\">");
            sb.append("<i class=\"icon-pencil\" style=\"margin-left:-5px;\"></i></a>");

            // Colonne Button Delete
            sb.append("</td><td style=\"width:5%;text-align:center\">");
            sb.append("<a href=\"");
            sb.append(req.getContextPath());
            sb.append(req.getServletPath());
            sb.append("?op=" + OP_RMV_FEATURE + "&" + FEATID + "=" + uid.getKey());
            sb.append("\" style=\"width:6px;\" class=\"btn\">");
            sb.append("<i class=\"icon-trash\" style=\"margin-left:-5px;\"></i>");
            sb.append("</a>");
            sb.append("</td></tr>");
        }
        return sb.toString();
    }

    /**
     * Render group list block.
     * 
     * @param ff4j
     *            target ff4j.
     * @return list of group
     */
    private static String renderGroupList(FF4j ff4j, String modalId) {
        StringBuilder sb = new StringBuilder();
        if (null != ff4j.getFeatureStore().readAllGroups()) {
            for (String group : ff4j.getFeatureStore().readAllGroups()) {
                sb.append("<li><a href=\"#\" onclick=\"\\$('\\#" + modalId + " \\#groupName').val('");
                sb.append(group);
                sb.append("');\">");
                sb.append(group);
                sb.append("</a></li>");
            }
        }
        return sb.toString();
    }

    /**
     * Render a permission list.
     *
     * @param ff4j
     *            reference to curent ff4j instance
     * @return string representing the list of permissions
     */
    private static String renderPermissionList(FF4j ff4j) {
        StringBuilder sb = new StringBuilder("<br/>");
        if (null != ff4j.getAuthorizationsManager()) {
            for (String permission : ff4j.getAuthorizationsManager().listAllPermissions()) {
                sb.append("\r\n<br/>&nbsp;&nbsp;&nbsp;<input type=\"checkbox\" ");
                sb.append(" name=\"" + PREFIX_CHECKBOX + permission + "\"");
                sb.append(" id=\"" + PREFIX_CHECKBOX + permission + "\" >&nbsp;");
                sb.append(permission);
            }
        }
        return sb.toString();
    }

    /**
     * Load the CSS File As String.
     *
     * @return CSS File
     */
    private static final String getCSS() {
        if (null == cssContent) {
            cssContent = loadFileAsString(RESOURCE_CSS_FILE);
        }
        return cssContent;
    }

    /**
     * Load the JS File As String.
     *
     * @return JS File
     */
    private static final String getJS() {
        if (null == jsContent) {
            jsContent = loadFileAsString(RESOURCE_JS_FILE);
        }
        return jsContent;
    }

    /**
     * Utils method to load a file as String.
     *
     * @param fileName
     *            target file Name.
     * @return target file content as String
     */
    private static String loadFileAsString(String fileName) {
        InputStream in = ConsoleRenderer.class.getClassLoader().getResourceAsStream(fileName);
        if (in == null) {
            throw new IllegalArgumentException("Cannot load file " + fileName + " from classpath");
        }
        Scanner currentScan = null;
        StringBuilder strBuilder = new StringBuilder();
        try {
            currentScan = new Scanner(in, UTF8_ENCODING);
            while (currentScan.hasNextLine()) {
                strBuilder.append(currentScan.nextLine());
                strBuilder.append(NEW_LINE);
            }
        } finally {
            if (currentScan != null) {
                currentScan.close();
            }
        }
        return strBuilder.toString();
    }

}
