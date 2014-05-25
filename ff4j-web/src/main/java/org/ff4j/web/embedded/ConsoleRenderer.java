package org.ff4j.web.embedded;

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
import java.util.Map;
import java.util.Scanner;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.core.FlippingStrategy;

/**
 * Used to build GUI Interface for feature flip servlet. It contains gui component render and parmeters
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public final class ConsoleRenderer implements ConsoleConstants {

    /** Cache for page blocks. */
    private static String htmlTemplate = null;

    /** Load CSS. */
    private static String cssContent = null;

    /** Load JS. */
    private static String jsContent = null;

    /** Cache for page blocks. */
    static final String TABLE_FEATURES_FOOTER = "" + "</tbody></table></form></fieldset>";

    /** fin de ligne. **/
    static final String END_OF_LINE = "\r\n";

    /**
     * Load HTML template file and substitute by current URL context path
     * 
     * @param req
     *            current http request
     * @return current text part as string
     */
    static final String getTemplate(HttpServletRequest req) {
        if (htmlTemplate == null || htmlTemplate.isEmpty()) {
            String ctx = req.getContextPath() + req.getServletPath() + "";
            htmlTemplate = loadFileAsString(TEMPLATE_FILE).replaceAll("\\{" + KEY_SERVLET_CONTEXT + "\\}", ctx);
        }
        return htmlTemplate;
    }

    /**
     * Produce the rows of the Feature Table.
     *
     * @param currentElement
     * @return
     */
    static final String renderFeatureRows(FF4j ff4j, HttpServletRequest req) {
        StringBuilder sb = new StringBuilder();
        final Map < String, Feature> mapOfFeatures = ff4j.getFeatures();
        for(String uid : mapOfFeatures.keySet()) {
            Feature currentFeature = mapOfFeatures.get(uid);
            sb.append("<tr>" + END_OF_LINE);
            
            // Column with uid and description as tooltip
            sb.append("<td><a class=\"ff4j-tooltip\" tooltip=\"");
            sb.append(currentFeature.getDescription());
            sb.append("\">");
            sb.append(currentFeature.getUid());
            sb.append("</a>");
            
            // Colonne Group
            sb.append("</td><td>&nbsp;");
            if (currentFeature.getGroup() != null) {
                sb.append(currentFeature.getGroup());
            }
            
            // Colonne Permissions
            sb.append("</td><td>&nbsp;");
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
            }
            
            // Colonne Strategy
            sb.append("</td><td>&nbsp;");
            FlippingStrategy fs = currentFeature.getFlippingStrategy();
            if (null != fs) {
                sb.append(fs.getClass().getCanonicalName());
                sb.append("<br/>&nbsp;" + fs.getInitParams());
            }
            
            // Colonne 'Holy' Toggle
            sb.append("</td><td style=\"width:8%;text-align:center\">");
            sb.append("<label class=\"switch switch-green\">");
            sb.append("<input id=\"tick-" + currentFeature.getUid() + "\" type=\"checkbox\" class=\"switch-input\"");
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
            sb.append("data-desc=\"" + currentFeature.getDescription()
                    + "\" style=\"width:6px;\" class=\"open-EditFlipDialog btn\">");
            sb.append("<i class=\"icon-pencil\" style=\"margin-left:-5px;\"></i></a>");

            // Colonne Button Delete
            sb.append("</td><td style=\"width:5%;text-align:center\">");
            sb.append("<a href=\"");
            sb.append(req.getContextPath());
            sb.append(req.getServletPath());
            sb.append("?op=" + OP_RMV_FEATURE + "&" + FEATID + "=" + uid);
            sb.append("\" style=\"width:6px;\" class=\"btn\">");
            sb.append("<i class=\"icon-trash\" style=\"margin-left:-5px;\"></i>");
            sb.append("</a>");
            sb.append("</td></tr>");
        }
        return null;
    }
    
    static String renderMessageBox(String message, String type) {
        StringBuilder sb = new StringBuilder();
        sb.append("<p><div class=\"alert alert-" + type + "\" style=\"margin-top:25px;margin-left:15px\" >");
        sb.append("<button type=\"button\" class=\"close\" data-dismiss=\"alert\">&times;</button>");
        sb.append(message);
        sb.append(ConsoleRenderer.class.getPackage().getImplementationVersion());
        sb.append("</div>");
        return sb.toString();
    }

    /**
     * Load the CSS File As String.
     *
     * @return CSS File
     */
    static final String getCSS() {
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
    static final String getJS() {
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
