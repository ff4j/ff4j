package org.ff4j.web.embedded;

/*
 * #%L AdministrationConsoleServlet.java (ff4j-web) by Cedrick LUNVEN %% Copyright (C) 2013 Ff4J %% Licensed under the Apache
 * License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of
 * the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.apache.commons.io.FilenameUtils;
import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureXmlParser;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Unique Servlet to manage FlipPoints and security
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class ConsoleServlet extends HttpServlet implements ConsoleConstants {

    /** Buffer size. */
    private static final int BUFFER_SIZE = 4096;

    /** serial number. */
    private static final long serialVersionUID = -3982043895954284269L;

    /** Parameter for this Servlet. */
    private static final String PROVIDER_PARAM_NAME = "ff4jProvider";

    /** Logger for this class. */
    public static Logger LOGGER = LoggerFactory.getLogger("FF4J");

    /** initializing ff4j provider. */
    private ConsoleFF4JProvider ff4jProvider = null;
    
    /** instance of ff4j. */
    private FF4j ff4j = null;

    /**
     * Servlet initialization, init FF4J from "ff4jProvider" attribute Name.
     *
     * @param servletConfig
     *            current {@link ServletConfig} context
     * @throws ServletException
     *             error during servlet initialization
     */
    @Override
    public void init(ServletConfig servletConfig) throws ServletException {
        LOGGER.debug("Initializing Embedded Servlet");
        String className = servletConfig.getInitParameter(PROVIDER_PARAM_NAME);
        try {
            Class<?> c = Class.forName(className);
            Object o = c.newInstance();
            ff4jProvider = (ConsoleFF4JProvider) o;
            LOGGER.info("FF4J Provider has been successfully loaded with {}", className);
        } catch (ClassNotFoundException e) {
            throw new IllegalArgumentException("Cannot load ff4jProvider as " + ff4jProvider, e);
        } catch (InstantiationException e) {
            throw new IllegalArgumentException("Cannot instanciate  " + ff4jProvider + " as ff4jProvider", e);
        } catch (IllegalAccessException e) {
            throw new IllegalArgumentException("No public constructor for  " + ff4jProvider + " as ff4jProvider", e);
        } catch (ClassCastException ce) {
            throw new IllegalArgumentException("ff4jProvider expected instance of " + ConsoleFF4JProvider.class, ce);
        }
        
        // Put the FF4J in ApplicationScope (useful for tags)
        ff4j = ff4jProvider.getFF4j();
        servletConfig.getServletContext().setAttribute(FF4J_SESSIONATTRIBUTE_NAME, ff4j);
        LOGGER.debug("Servlet has been initialized and ff4j store in session with {} ", ff4j.getFeatures().size());
    }

    /** {@inheritDoc} */
    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException {

        String message = null;
        String messagetype = "info";

        // Routing on pagename
        try {

            // Serve static resource file as CSS and Javascript
            String resources = req.getParameter(RESOURCE);
            if (resources != null && !resources.isEmpty()) {
                LOGGER.debug("GET Access to console to retrieve resource {}", resources);
                if (RESOURCE_CSS_PARAM.equalsIgnoreCase(resources)) {
                    res.setContentType(CONTENT_TYPE_CSS);
                    res.getWriter().println(ConsoleRenderer.getCSS());
                    LOGGER.debug("Retrieving CSS");
                    return;
                } else if (RESOURCE_JS_PARAM.equalsIgnoreCase(resources)) {
                    res.setContentType(CONTENT_TYPE_JS);
                    res.getWriter().println(ConsoleRenderer.getJS());
                    LOGGER.debug("Retrieving JS");
                    return;
                }
                
            }

            // Serve operation from GET
            String operation = req.getParameter(OPERATION);
            if (operation != null && !operation.isEmpty()) {
                LOGGER.debug("GET Access to console for operation {}", resources);
                if (OP_DISABLE.equalsIgnoreCase(operation)) {
                    opDisableFeature(req);
                    message = buildMessage(req.getParameter(FEATID), "DISABLED");
                } else if (OP_ENABLE.equalsIgnoreCase(operation)) {
                    opEnableFeature(req);
                    message = buildMessage(req.getParameter(FEATID), "ENABLED");
                } else if (OP_EDIT_FEATURE.equalsIgnoreCase(operation)) {
                    opUpdateFeatureDescription(req);
                    message = buildMessage(req.getParameter(FEATID), "UPDATED");
                } else if (OP_CREATE_FEATURE.equalsIgnoreCase(operation)) {
                    opAddNewFeature(req);
                    message = buildMessage(req.getParameter(FEATID), "ADDED");
                } else if (OP_RMV_FEATURE.equalsIgnoreCase(operation)) {
                    opDeleteFeature(req);
                    message = buildMessage(req.getParameter(FEATID), "DELETED");
                } else if (OP_ADD_ROLE.equalsIgnoreCase(operation)) {
                    opAddRoleToFeature(req);
                    message = "Role <b>" + req.getParameter(ROLE) + "</b> has been successfully added to flipPoint <b>"
                            + req.getParameter(FEATID) + " </b>";
                } else if (OP_RMV_ROLE.equalsIgnoreCase(operation)) {
                    opRemoveRoleFromFeature(req);
                    message = "Role <b>" + req.getParameter(ROLE) + "</b> has been successfully removed from flipPoint <b>"
                            + req.getParameter(FEATID) + " </b>";
                } else if (OP_EXPORT.equalsIgnoreCase(operation)) {
                    buildResponseForExportFeature(res);
                }
            }

        } catch (Exception e) {
            // Any Error is trapped and display in the console
            messagetype = "error";
            message = e.getMessage();
        }

        // Default page rendering (table)
        renderPage(req, res, message, messagetype);
    }

    /**
     * Build info messages.
     * 
     * @param featureName
     *            target feature name
     * @param operationd
     *            target operationId
     * @return
     */
    private String buildMessage(String featureName, String operationId) {
        return String.format("Feature <b>%s</b> has been successfully %s", featureName, operationId);
    }

    /**
     * Build Http response when invoking export features.
     * 
     * @param res
     *            http response
     * @throws IOException
     *             error when building response
     */
    private void buildResponseForExportFeature(HttpServletResponse res) throws IOException {
        InputStream in = new FeatureXmlParser().exportFeatures(getFf4j().getStore().readAll());
        ServletOutputStream sos = null;
        try {
            sos = res.getOutputStream();
            res.setContentType("text/xml");
            res.setHeader("Content-Disposition", "attachment; filename=\"ff4j.xml\"");
            // res.setContentLength()
            byte[] bbuf = new byte[BUFFER_SIZE];
            int length = 0;
            while ((in != null) && (length != -1)) {
                length = in.read(bbuf);
                sos.write(bbuf, 0, length);
            }
        } finally {
            if (in != null) {
                in.close();
            }
            if (sos != null) {
                sos.flush();
                sos.close();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException {
        String message = null;
        String messagetype = "error";
        try {
            List<FileItem> items = new ServletFileUpload(new DiskFileItemFactory()).parseRequest(req);
            for (FileItem item : items) {
                if (item.isFormField()) {
                    if (OPERATION.equalsIgnoreCase(item.getFieldName())) {
                        // String operation = item.getString();
                        // Proceed here action accessing through POST
                    }
                } else if (FLIPFILE.equalsIgnoreCase(item.getFieldName())) {
                    String filename = FilenameUtils.getName(item.getName());
                    if (filename.toLowerCase().endsWith("xml")) {
                        opImportFile(item.getInputStream());
                    } else {
                        messagetype = "error";
                        message = "Invalid FILE, must be CSV, XML or PROPERTIES files";
                    }
                }
            }
        } catch (Exception e) {
            message = e.getMessage();
        }
        renderPage(req, res, message, messagetype);
    }

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
    private void renderPage(HttpServletRequest req, HttpServletResponse res, String msg, String msgType)
            throws IOException {
        res.setContentType(CONTENT_TYPE_HTML);
        PrintWriter out = res.getWriter();

        // Header of the page
        String htmlContent = ConsoleRenderer.renderTemplate(req);

        // Subsctitution FEATURE_ROWS
        final String msgBox = ConsoleRenderer.renderMessageBox(msg, msgType);
        htmlContent = htmlContent.replaceAll("\\{" + KEY_ALERT_MESSAGE + "\\}", msgBox);

        // Subsctitution FEATURE_ROWS
        final String rows = ConsoleRenderer.renderFeatureRows(ff4j, req);
        htmlContent = htmlContent.replaceAll("\\{" + KEY_FEATURE_ROWS + "\\}", rows);

        // Substitution GROUP_LIST
        htmlContent = htmlContent.replaceAll("\\{" + KEY_GROUP_LIST + "\\}", "");

        out.println(htmlContent);

    }

    /**
     * User action to enable a Feature.
     * 
     * @param req
     *            http request containing operation parameters
     */
    private void opEnableFeature(HttpServletRequest req) {
        final String featureId = req.getParameter(FEATID);
        if (featureId != null && !featureId.isEmpty()) {
            getFf4j().enable(featureId);
        }
    }

    /**
     * User action to disable a Feature.
     * 
     * @param req
     *            http request containing operation parameters
     */
    private void opDisableFeature(HttpServletRequest req) {
        final String featureId = req.getParameter(FEATID);
        if (featureId != null && !featureId.isEmpty()) {
            getFf4j().disable(featureId);
        }
    }

    /**
     * User action to create a new Feature.
     * 
     * @param req
     *            http request containing operation parameters
     */
    private void opAddNewFeature(HttpServletRequest req) {
        final String featureId = req.getParameter(FEATID);
        final String featureDesc = req.getParameter(DESCRIPTION);
        if (featureId != null && !featureId.isEmpty()) {
            Feature fp = new Feature(featureId, false, featureDesc);
            getFf4j().getStore().create(fp);
        }
    }

    /**
     * User action to delete a new Feature.
     * 
     * @param req
     *            http request containing operation parameters
     */
    private void opDeleteFeature(HttpServletRequest req) {
        final String featureId = req.getParameter(FEATID);
        if (featureId != null && !featureId.isEmpty()) {
            getFf4j().getStore().delete(featureId);
        }
    }

    /**
     * User action to update a target feature's description.
     * 
     * @param req
     *            http request containing operation parameters
     */
    private void opUpdateFeatureDescription(HttpServletRequest req) {
        final String featureId = req.getParameter(FEATID);
        final String description = req.getParameter(DESCRIPTION);
        if (featureId != null && !featureId.isEmpty()) {
            Feature fp = getFf4j().getStore().read(featureId);
            fp.setDescription(description);
            getFf4j().getStore().update(fp);
        }
    }

    /**
     * User action to add a role to feature.
     * 
     * @param req
     *            http request containing operation parameters
     */
    private void opAddRoleToFeature(HttpServletRequest req) {
        final String flipId = req.getParameter(FEATID);
        final String roleName = req.getParameter(ROLE);
        getFf4j().getStore().grantRoleOnFeature(flipId, roleName);
    }

    /**
     * User action to remove a role from feature.
     * 
     * @param req
     *            http request containing operation parameters
     */
    private void opRemoveRoleFromFeature(HttpServletRequest req) {
        final String flipId = req.getParameter(FEATID);
        final String roleName = req.getParameter(ROLE);
        getFf4j().getStore().removeRoleFromFeature(flipId, roleName);
    }

    /**
     * User action to import Features from a properties files.
     * 
     * @param in
     *            inpustream from configuration file
     * @throws IOException
     *             Error raised if the configuration cannot be read
     */
    private void opImportFile(InputStream in) throws IOException {
        Map<String, Feature> mapsOfFeat = new FeatureXmlParser().parseConfigurationFile(in);
        for (Entry<String, Feature> feature : mapsOfFeat.entrySet()) {
            if (getFf4j().getStore().exist(feature.getKey())) {
                getFf4j().getStore().update(feature.getValue());
            } else {
                getFf4j().getStore().create(feature.getValue());
            }
        }
    }

    /**
     * Getter accessor for attribute 'ff4j'.
     * 
     * @return current value of 'ff4j'
     */
    public FF4j getFf4j() {
        if (ff4j == null) {
            throw new IllegalStateException("Console Servlet has not been initialized, please set 'load-at-startup' to 1");
        }
        return ff4j;
    }

}
