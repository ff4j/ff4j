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
import java.util.Map.Entry;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeSet;

import javax.servlet.http.HttpServletRequest;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;

/**
 * Used to build GUI Interface for feature flip servlet. It contains gui component render and parmeters
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public final class ConsoleRenderer implements ConsoleConstants {

    /** Cache for page blocks. */
    private static String modalImportFeatures = null;

    /** Cache for page blocks. */
    private static String modalEditFeature = null;

    /** Cache for page blocks. */
    private static String htmlModalNewFeature = null;

    /** Cache for page blocks. */
    private static String htmlHeader = null;

    /** Cache for page blocks. */
    private static String htmlTableHeader = null;

    /** Load CSS. */
    private static String cssContent = null;

    /** Load JS. */
    private static String jsContent = null;

    /** Cache for page blocks. */
    static final String TABLE_FEATURES_FOOTER = "" + "</tbody></table></form></fieldset>";


    /**
     * Rendering of part of the screen.
     * 
     * @param req
     *            current http request
     * @return current text part as string
     */
    static final String renderHeader(HttpServletRequest req) {
        if (htmlHeader == null || htmlHeader.isEmpty()) {
            String ctx = req.getContextPath() + req.getServletPath() + "";
            htmlHeader = loadFileAsString(TEMPLATE_FILE);
            htmlHeader = htmlHeader.replaceAll("\\{" + KEY_CTX + "\\}", ctx);
        }
        return htmlHeader;
    }

    /**
     * Rendering of part of the screen.
     * 
     * @param req
     *            current http request
     * @return current text part as string
     */
    static final String renderTableHeader(HttpServletRequest req) {
        if (htmlTableHeader == null || htmlTableHeader.isEmpty()) {
            htmlHeader = loadFileAsString(TEMPLATE_TABLE_HEADER);
        }
        return htmlTableHeader;
    }

    static String renderModalEditFlip(HttpServletRequest req) {
        if (modalEditFeature == null || modalEditFeature.isEmpty()) {
            StringBuilder sb = new StringBuilder();
            sb.append("<form class=\"form-horizontal\" action=\"" + req.getContextPath() + req.getServletPath()
                    + "\" method=\"GET\" >" + "<div class=\"modal hide\" id=\"modalEditFlip\" ");
            sb.append("     tabindex=\"-1\" role=\"dialog\" aria-labelledby=\"myModalLabel\" aria-hidden=\"true\">");
            sb.append("" + "<div class=\"modal-header\">" + "   <button class=\"close\" data-dismiss=\"modal\">x</button>"
                    + "   <h3 id=\"myModalLabel\">Edit Feature</h3>" + "</div>");
            sb.append("" + "<div class=\"modal-body\">" + " <div class=\"control-group\">"
                    + "  <label class=\"control-label\" for=\"uid\">Flipoint Name</label>" + "  <div class=\"controls\">"
                    + "   <input type=\"text\" name=\"uid\" id=\"" + FEATID
                    + "\" style=\"width:250px;height:30px;\" readonly=\"readonly\" />" + "  </div>" + " </div>"
                    + " <div class=\"control-group\">" + "  <label class=\"control-label\" for=\"desc\">Description</label>"
                    + "  <div class=\"controls\">" + "    <input type=\"text\" name=\"desc\" id=\"" + DESCRIPTION
                    + "\" style=\"width:250px;height:30px;\" />" + "  </div>" + " </div>" + "</div>");
            sb.append(""
                    + "<div class=\"modal-footer\">"
                    + "<button class=\"btn btn\" data-dismiss=\"modal\"><i class=\"icon-remove\" ></i>&nbsp;Cancel</button>"
                    + "<button class=\"btn btn-primary\" type=\"submit\"><i class=\"icon-ok icon-white\" ></i>&nbsp;Save changes</button>"
                    + "</div>\n" + "<script type=\"text/javascript\" >\n"
                    + "$(document).on(\"click\", \".open-EditFlipDialog\", function () {\n"
                    + "var flipId = $(this).data('id');\n" + "var desc   = $(this).data('desc');\n"
                    + "$(\".modal-body #uid\").val(flipId);\n" + "$(\".modal-body #desc\").val(desc);\n"
                    + "$(\".modal-body #desc\").focus();\n" + "});\n" + "</script>\n"
                    + " <input type=\"hidden\" name=\"op\" value=\"" + OP_EDIT_FEATURE + "\"  />" + "</div>" + "</form>");
            modalEditFeature = sb.toString();
        }
        return modalEditFeature;
    }

    /**
     * 
     * @param req
     * @return
     */
    static String renderModalNewFlipPoint(HttpServletRequest req) {
        if (htmlModalNewFeature == null || htmlModalNewFeature.isEmpty()) {
            StringBuilder sb = new StringBuilder();
            sb.append("<form class=\"form-horizontal\" action=\"" + req.getContextPath() + req.getServletPath()
                    + "\" method=\"GET\" >" + "<div class=\"modal hide\" id=\"modalAddFlip\" ");
            sb.append("     tabindex=\"-1\" role=\"dialog\" aria-labelledby=\"myModalLabel\" aria-hidden=\"true\">");
            sb.append("" + "<div class=\"modal-header\">" + "   <button class=\"close\" data-dismiss=\"modal\">x</button>"
                    + "   <h3 id=\"myModalLabel\">Add Feature</h3>" + "</div>");
            sb.append("" + "<div class=\"modal-body\">" + " <div class=\"control-group\">"
                    + "  <label class=\"control-label\" for=\"uid\">Flipoint Name</label>" + "  <div class=\"controls\">"
                    + "   <input type=\"text\" name=\"uid\" id=\"" + FEATID + "\" style=\"width:250px;height:30px;\" required/>"
                    + "  </div>" + " </div>" + " <div class=\"control-group\">"
                    + "  <label class=\"control-label\" for=\"desc\">Description</label>" + "  <div class=\"controls\">"
                    + "    <input type=\"text\" name=\"desc\" id=\"" + DESCRIPTION + "\" style=\"width:250px;height:30px;\" />"
                    + "  </div>" + " </div>" + "</div>");
            sb.append(""
                    + "<div class=\"modal-footer\" >"
                    + "<button class=\"btn btn\" data-dismiss=\"modal\"><i class=\"icon-remove\" ></i>&nbsp;Cancel</button>"
                    + "<button class=\"btn btn-primary\" type=\"submit\"><i class=\"icon-ok icon-white\" ></i>&nbsp;Add New </button>"
                    + "</div>\n" + "<script type=\"text/javascript\" >\n"
                    + "$(document).on(\"click\", \".open-AddFlipDialog\", function () {\n" + "$(\".modal-body #uid\").focus();\n"
                    + "});\n" + "</script>\n" + " <input type=\"hidden\" name=\"op\" value=\"" + OP_ADD_FEATURE + "\"  />"
                    + "</div>" + "</form>");
            htmlModalNewFeature = sb.toString();
        }
        return htmlModalNewFeature;
    }

    /**
     * Produce HTML code of a modal which contain a form to upload file.
     * 
     * @param req
     *            current HTTP request (getting relative servlet path)
     * @return HTML code to render a import button
     */
    static String renderModalImportFlipPoints(HttpServletRequest req) {
        if (modalImportFeatures == null || modalImportFeatures.isEmpty()) {
            StringBuilder sb = new StringBuilder();
            sb.append("<div class=\"modal hide fade\" id=\"modalImportFlip\" ");
            sb.append("     tabindex=\"-1\" role=\"dialog\" aria-labelledby=\"myModalLabel\" aria-hidden=\"true\">");
            sb.append("" + "<form action=\"" + req.getContextPath() + req.getServletPath()
                    + "\" method=\"POST\" enctype=\"multipart/form-data\" >" + "<div class=\"modal-header\">"
                    + "   <button class=\"close\" data-dismiss=\"modal\">x</button>"
                    + "   <h3 id=\"myModalLabel\">Import Features from xml file</h3>" + "</div>");
            sb.append("" + "<div class=\"modal-body\">"
                    + "<p/>Please choose a <b>CSV</b>, <b>XML</b> or <b>PROPERTIES</b> FF4J configuration files : </p>"
                    + "<input type=\"hidden\" name=\"op\" value=\"" + OP_IMPORT + "\"  />"
                    + "<input type=\"file\" name=\"flipFile\"  />" + "</div>");
            sb.append(""
                    + "<div class=\"modal-footer\">"
                    + "<button class=\"btn btn\" data-dismiss=\"modal\"><i class=\"icon-remove\" ></i>&nbsp;Cancel</button>"
                    + "<button class=\"btn btn-primary\" type=\"submit\"><i class=\"icon-file icon-white\" ></i>&nbsp;Import </button>"
                    + "</div>\n" + "</form></div>");
            modalImportFeatures = sb.toString();
        }
        return modalImportFeatures;
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

    static String renderButtonDeleteFeature(HttpServletRequest req, String uid) {
        StringBuilder strBuilder = new StringBuilder("<a href=\"");
        strBuilder.append(req.getContextPath());
        strBuilder.append(req.getServletPath());
        strBuilder.append("?op=" + OP_RMV_FEATURE + "&" + FEATID + "=" + uid);
        strBuilder.append("\" style=\"width:6px;\" class=\"btn\">");
        strBuilder.append("<i class=\"icon-trash\" style=\"margin-left:-5px;\"></i>");
        strBuilder.append("</a>");
        return strBuilder.toString();
    }

    static String renderButtonEditFeature(FF4j ff4j, HttpServletRequest req, String uid) {
        StringBuilder strBuilder = new StringBuilder("<a data-toggle=\"modal\" href=\"#modalEditFlip\"");
        String desc = ff4j.getStore().read(uid).getDescription();
        strBuilder.append("\" data-id=\"" + uid + "\" data-desc=\"" + desc
                + "\" style=\"width:6px;\" class=\"open-EditFlipDialog btn\">");
        strBuilder.append("<i class=\"icon-pencil\" style=\"margin-left:-5px;\"></i>");
        strBuilder.append("</a>");
        return strBuilder.toString();
    }

    static String renderButtonUserRole(FF4j ff4j, HttpServletRequest req, Feature fp) {
        Set<String> setOfRoles = new TreeSet<String>();
        if (ff4j.getAuthorizationsManager().getEveryOneRoles() != null) {
            setOfRoles.addAll(ff4j.getAuthorizationsManager().getEveryOneRoles());
        }
        StringBuilder strBuilder = new StringBuilder("<div class=\"btn-group\">");
        strBuilder.append("<button class=\"btn\"><i class=\"icon-user\"></i></button>");
        strBuilder
                .append("<button class=\"btn dropdown-toggle\" data-toggle=\"dropdown\"><span class=\"caret\"></span></button>");
        strBuilder.append("<ul class=\"dropdown-menu\" role=\"menu\">");
        for (String role : setOfRoles) {
            StringBuilder link = new StringBuilder("<li><a href=\"");
            link.append(req.getContextPath());
            link.append(req.getServletPath());
            if (fp.getAuthorizations().contains(role)) {
                link.append("?op=" + OP_RMV_ROLE);
                link.append("&" + FEATID + "=" + fp.getUid());
                link.append("&" + ROLE + "=" + role);
                link.append("\" style=\"color:#00AA00\">");
            } else {
                link.append("?op=" + OP_ADD_ROLE);
                link.append("&" + FEATID + "=" + fp.getUid());
                link.append("&" + ROLE + "=" + role);
                link.append("\" style=\"color:#AAAAAA\">");
            }
            strBuilder.append(link.toString());
            strBuilder.append(role);
            strBuilder.append("</li>");
        }
        strBuilder.append(" </ul>");
        strBuilder.append("</div>");
        return strBuilder.toString();
    }

    /**
     * Render a bootstrap button.
     *
     * @param req
     * @param label
     * @param color
     * @param action
     * @param pp
     * @param icon
     * @return
     */
    static String renderElementButton(HttpServletRequest req, String label, String color, String action, Map<String, String> pp,
            String icon) {
        StringBuilder strBuilder = new StringBuilder("<a href=\"");
        strBuilder.append(req.getContextPath());
        strBuilder.append(req.getServletPath());
        strBuilder.append("?op=" + action);
        if (pp != null && !pp.isEmpty()) {
            for (Entry<String, String> param : pp.entrySet()) {
                strBuilder.append("&" + param.getKey() + "=" + param.getValue());
            }
        }
        if (icon != null && !icon.isEmpty()) {
            strBuilder.append("\" style=\"width:70px\" class=\"btn btn-" + color + "\">");
            strBuilder.append("<i class=\"icon-" + icon + "\"></i>&nbsp;&nbsp;");
        } else {
            strBuilder.append("\" style=\"width:60px\" class=\"btn btn-" + color + "\">");
        }
        strBuilder.append(label);
        strBuilder.append("</a>");
        return strBuilder.toString();
    }

    /**
     * Load the CSS File As String.
     *
     * @return CSS File
     */
    static final String getCSS() {
        if (null == cssContent) {
            cssContent = loadFileAsString(CSS_FILE);
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
            jsContent = loadFileAsString(JS_FILE);
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
