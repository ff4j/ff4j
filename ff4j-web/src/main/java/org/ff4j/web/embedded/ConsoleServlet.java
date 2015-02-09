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
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

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
import org.ff4j.core.FlippingStrategy;
import org.ff4j.web.api.FF4JProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Unique Servlet to manage FlipPoints and security
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class ConsoleServlet extends HttpServlet implements ConsoleConstants {

    /** serial number. */
    private static final long serialVersionUID = -3982043895954284269L;

    /** Logger for this class. */
    public static Logger LOGGER = LoggerFactory.getLogger("FF4J");

    /** instance of ff4j. */
    private FF4j ff4j = null;

    /** initializing ff4j provider. */
    private FF4JProvider ff4jProvider = null;

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
            ff4jProvider = (FF4JProvider) o;
            LOGGER.info("  __  __ _  _   _ ");
            LOGGER.info(" / _|/ _| || | (_)");
            LOGGER.info("| |_| |_| || |_| |");
            LOGGER.info("|  _|  _|__   _| |");
            LOGGER.info("|_| |_|    |_|_/ |");
            LOGGER.info("             |__/   Embedded Console - v" + getClass().getPackage().getImplementationVersion());
            LOGGER.info(" ");
            LOGGER.info("ff4j context has been successfully initialized - {} feature(s)", ff4jProvider.getFF4j()
                    .getFeatures().size());
        } catch (ClassNotFoundException e) {
            throw new IllegalArgumentException("Cannot load ff4jProvider as " + ff4jProvider, e);
        } catch (InstantiationException e) {
            throw new IllegalArgumentException("Cannot instanciate  " + ff4jProvider + " as ff4jProvider", e);
        } catch (IllegalAccessException e) {
            throw new IllegalArgumentException("No public constructor for  " + ff4jProvider + " as ff4jProvider", e);
        } catch (ClassCastException ce) {
            throw new IllegalArgumentException("ff4jProvider expected instance of " + FF4JProvider.class, ce);
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
                if (OP_DISABLE.equalsIgnoreCase(operation)) {
                    opDisableFeature(req);
                    res.setContentType(CONTENT_TYPE_HTML);
                    PrintWriter out = res.getWriter();
                    String targetMessage = buildMessage(req.getParameter(FEATID), "DISABLED");
                    out.println(ConsoleRenderer.renderMessageBox(targetMessage, "info"));
                    return;
                } else if (OP_ENABLE.equalsIgnoreCase(operation)) {
                    opEnableFeature(req);
                    res.setContentType(CONTENT_TYPE_HTML);
                    PrintWriter out = res.getWriter();
                    out.println(ConsoleRenderer.renderMessageBox(buildMessage(req.getParameter(FEATID), "ENABLED"), "info"));
                    return;
                } else if (OP_RMV_FEATURE.equalsIgnoreCase(operation)) {
                    opDeleteFeature(req);
                    message = buildMessage(req.getParameter(FEATID), "DELETED");
                } else if (OP_EXPORT.equalsIgnoreCase(operation)) {
                    opExportFile(res);
                    message = "Feature have been success fully exported";
                    return;
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
     * Build info messages.
     * 
     * @param featureName
     *            target feature name
     * @param operationd
     *            target operationId
     * @return
     */
    private String buildMessageGroup(String groupName, String operationId) {
        return String.format("Group <b>%s</b> has been successfully %s", groupName, operationId);
    }

    /** {@inheritDoc} */
    @Override
    protected void doPost(HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException {
        String message = null;
        String messagetype = "info";
        try {

            if (ServletFileUpload.isMultipartContent(req)) {
                List<FileItem> items = new ServletFileUpload(new DiskFileItemFactory()).parseRequest(req);
                for (FileItem item : items) {
                    if (item.isFormField()) {
                        if (OPERATION.equalsIgnoreCase(item.getFieldName())) {
                            LOGGER.info("Processing action : " + item.getString());
                        }
                    } else if (FLIPFILE.equalsIgnoreCase(item.getFieldName())) {
                        String filename = FilenameUtils.getName(item.getName());
                        if (filename.toLowerCase().endsWith("xml")) {
                            opImportFile(item.getInputStream());
                            message = "The file <b>" + filename + "</b> has been successfully imported";
                        } else {
                            messagetype = "error";
                            message = "Invalid FILE, must be CSV, XML or PROPERTIES files";
                        }
                    }
                }

            } else {
                String operation = req.getParameter(OPERATION);
                if (operation != null && !operation.isEmpty()) {

                    if (OP_EDIT_FEATURE.equalsIgnoreCase(operation)) {
                        opUpdateFeatureDescription(req);
                        message = buildMessage(req.getParameter(FEATID), "UPDATED");

                    } else if (OP_CREATE_FEATURE.equalsIgnoreCase(operation)) {
                        opCreateFeature(req);
                        message = buildMessage(req.getParameter(FEATID), "ADDED");

                    } else if (OP_TOGGLE_GROUP.equalsIgnoreCase(operation)) {
                        String groupName = req.getParameter(GROUPNAME);
                        if (groupName != null && !groupName.isEmpty()) {
                            String operationGroup = req.getParameter(SUBOPERATION);
                            if (OP_ENABLE.equalsIgnoreCase(operationGroup)) {
                                getFf4j().getStore().enableGroup(groupName);
                                message = buildMessageGroup(groupName, "ENABLED");
                                LOGGER.info("Group '" + groupName + "' has been ENABLED.");
                            } else if (OP_DISABLE.equalsIgnoreCase(operationGroup)) {
                                getFf4j().getStore().disableGroup(groupName);
                                message = buildMessageGroup(groupName, "DISABLED");
                                LOGGER.info("Group '" + groupName + "' has been DISABLED.");
                            }
                        }
                    } else {
                        LOGGER.error("Invalid POST OPERATION" + operation);
                        messagetype = "error";
                        message = "Invalid REQUEST";
                    }
                }
            }

        } catch (Exception e) {
            messagetype = "error";
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
    private void renderPage(HttpServletRequest req, HttpServletResponse res, String msg, String msgType) throws IOException {
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
        String groups = ConsoleRenderer.renderGroupList(ff4j, MODAL_EDIT);
        htmlContent = htmlContent.replaceAll("\\{" + KEY_GROUP_LIST_EDIT + "\\}", groups);
        groups = groups.replaceAll(MODAL_EDIT, MODAL_CREATE);
        htmlContent = htmlContent.replaceAll("\\{" + KEY_GROUP_LIST_CREATE + "\\}", groups);
        groups = groups.replaceAll(MODAL_CREATE, MODAL_TOGGLE);
        htmlContent = htmlContent.replaceAll("\\{" + KEY_GROUP_LIST_TOGGLE + "\\}", groups);

        // Substitution PERMISSIONS
        final String permissions = ConsoleRenderer.renderPermissionList(ff4j);
        htmlContent = htmlContent.replaceAll("\\{" + KEY_PERMISSIONLIST + "\\}", permissions);

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
            LOGGER.info(featureId + " has been successfully enabled");
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
            LOGGER.info(featureId + " has been disabled");
        }
    }

    /**
     * User action to create a new Feature.
     * 
     * @param req
     *            http request containing operation parameters
     */
    private void opCreateFeature(HttpServletRequest req) {
        // uid
        final String featureId = req.getParameter(FEATID);
        if (featureId != null && !featureId.isEmpty()) {
            Feature fp = new Feature(featureId, false);

            // Description
            final String featureDesc = req.getParameter(DESCRIPTION);
            if (null != featureDesc && !featureDesc.isEmpty()) {
                fp.setDescription(featureDesc);
            }

            // GroupName
            final String groupName = req.getParameter(GROUPNAME);
            if (null != groupName && !groupName.isEmpty()) {
                fp.setGroup(groupName);
            }

            // Strategy
            final String strategy = req.getParameter(STRATEGY);
            if (null != strategy && !strategy.isEmpty()) {
                try {
                    Class<?> strategyClass = Class.forName(strategy);
                    FlippingStrategy fstrategy = (FlippingStrategy) strategyClass.newInstance();

                    final String strategyParams = req.getParameter(STRATEGY_INIT);
                    if (null != strategyParams && !strategyParams.isEmpty()) {
                        Map<String, String> initParams = new HashMap<String, String>();
                        String[] params = strategyParams.split(";");
                        for (String currentP : params) {
                            String[] cur = currentP.split("=");
                            if (cur.length < 2) {
                                throw new IllegalArgumentException("Invalid Syntax : param1=val1,val2;param2=val3,val4");
                            }
                            initParams.put(cur[0], cur[1]);
                        }
                        fstrategy.init(featureId, initParams);
                    }
                    fp.setFlippingStrategy(fstrategy);

                } catch (ClassNotFoundException e) {
                    throw new IllegalArgumentException("Cannot find strategy class", e);
                } catch (InstantiationException e) {
                    throw new IllegalArgumentException("Cannot instanciate strategy", e);
                } catch (IllegalAccessException e) {
                    throw new IllegalArgumentException("Cannot instanciate : no public constructor", e);
                }
            }

            // Permissions
            final String permission = req.getParameter(PERMISSION);
            if (null != permission && PERMISSION_RESTRICTED.equals(permission)) {
                @SuppressWarnings("unchecked")
                Map<String, Object> parameters = req.getParameterMap();
                Set<String> permissions = new HashSet<String>();
                for (String key : parameters.keySet()) {
                    if (key.startsWith(PREFIX_CHECKBOX)) {
                        permissions.add(key.replace(PREFIX_CHECKBOX, ""));
                    }
                }
                fp.setPermissions(permissions);
            }

            // Creation
            getFf4j().getStore().create(fp);
            LOGGER.info(featureId + " has been created");
        }
    }

    /**
     * User action to update a target feature's description.
     * 
     * @param req
     *            http request containing operation parameters
     */
    private void opUpdateFeatureDescription(HttpServletRequest req) {
        // uid
        final String featureId = req.getParameter(FEATID);
        if (featureId != null && !featureId.isEmpty()) {
            Feature fp = new Feature(featureId, false);

            // Description
            final String featureDesc = req.getParameter(DESCRIPTION);
            if (null != featureDesc && !featureDesc.isEmpty()) {
                fp.setDescription(featureDesc);
            }

            // GroupName
            final String groupName = req.getParameter(GROUPNAME);
            if (null != groupName && !groupName.isEmpty()) {
                fp.setGroup(groupName);
            }

            // Strategy
            final String strategy = req.getParameter(STRATEGY);
            if (null != strategy && !strategy.isEmpty()) {
                try {
                    Class<?> strategyClass = Class.forName(strategy);
                    FlippingStrategy fstrategy = (FlippingStrategy) strategyClass.newInstance();

                    final String strategyParams = req.getParameter(STRATEGY_INIT);
                    if (null != strategyParams && !strategyParams.isEmpty()) {
                        Map<String, String> initParams = new HashMap<String, String>();
                        String[] params = strategyParams.split(";");
                        for (String currentP : params) {
                            String[] cur = currentP.split("=");
                            if (cur.length < 2) {
                                throw new IllegalArgumentException("Invalid Syntax : param1=val1,val2;param2=val3,val4");
                            }
                            initParams.put(cur[0], cur[1]);
                        }
                        fstrategy.init(featureId, initParams);
                    }
                    fp.setFlippingStrategy(fstrategy);

                } catch (ClassNotFoundException e) {
                    throw new IllegalArgumentException("Cannot find strategy class", e);
                } catch (InstantiationException e) {
                    throw new IllegalArgumentException("Cannot instanciate strategy", e);
                } catch (IllegalAccessException e) {
                    throw new IllegalArgumentException("Cannot instanciate : no public constructor", e);
                }
            }

            // Permissions
            final String permission = req.getParameter(PERMISSION);
            if (null != permission && PERMISSION_RESTRICTED.equals(permission)) {
                @SuppressWarnings("unchecked")
                Map<String, Object> parameters = req.getParameterMap();
                Set<String> permissions = new HashSet<String>();
                for (String key : parameters.keySet()) {
                    if (key.startsWith(PREFIX_CHECKBOX)) {
                        permissions.add(key.replace(PREFIX_CHECKBOX, ""));
                    }
                }
                fp.setPermissions(permissions);
            }

            // Creation
            getFf4j().getStore().update(fp);
            LOGGER.info(featureId + " has been updated");
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
            LOGGER.info(featureId + " has been deleted");
        }
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
        LOGGER.info(mapsOfFeat.size() + " features have been imported.");
    }

    /**
     * Build Http response when invoking export features.
     * 
     * @param res
     *            http response
     * @throws IOException
     *             error when building response
     */
    private void opExportFile(HttpServletResponse res) throws IOException {
        Map<String, Feature> features = getFf4j().getStore().readAll();
        InputStream in = new FeatureXmlParser().exportFeatures(features);
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
            LOGGER.info(features.size() + " features have been exported.");
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
