package org.ff4j.web.embedded;

import static org.ff4j.web.embedded.ConsoleRenderer.renderMessageBox;
import static org.ff4j.web.embedded.ConsoleRenderer.renderPage;

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
import java.util.List;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.apache.commons.io.FilenameUtils;
import org.ff4j.FF4j;
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
    public Logger LOGGER = LoggerFactory.getLogger(getClass());

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
            LOGGER.info("ff4j context has been successfully initialized - {} feature(s)", ff4jProvider.getFF4j().getFeatures().size());
            
        } catch (ClassNotFoundException e) {
            throw new IllegalArgumentException("Cannot load ff4jProvider as " + ff4jProvider, e);
        } catch (InstantiationException e) {
            throw new IllegalArgumentException("Cannot instantiate  " + ff4jProvider + " as ff4jProvider", e);
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
    public void doGet(HttpServletRequest req, HttpServletResponse res)
    throws ServletException, IOException {
        String message = null;
        String messagetype = "info";
        // Routing on pagename
        try {
            
            // 'RSC' parameter will load some static resources
            if (ConsoleRenderer.renderResources(req, res)) return;

            // Serve operation from GET
            String operation = req.getParameter(OPERATION);
            String featureId = req.getParameter(FEATID);
            if (operation != null && !operation.isEmpty()) {
                
                // operation which do not required features
                if (OP_EXPORT.equalsIgnoreCase(operation)) {
                    ConsoleOperations.exportFile(ff4j, res);
                    message = "Feature have been success fully exported";
                    return;
                }                
                
                // Work on a feature ID
                if ((featureId != null) && (!featureId.isEmpty()) 
                        && getFf4j().getFeatureStore().exist(featureId)) {
                    
                    if (OP_DISABLE.equalsIgnoreCase(operation)) {
                        getFf4j().disable(featureId);
                        res.setContentType(CONTENT_TYPE_HTML);
                        res.getWriter().println(renderMessageBox(msg(featureId, "DISABLED"), "info"));
                        LOGGER.info(featureId + " has been disabled");
                        return;
                    } 
                    
                    if (OP_ENABLE.equalsIgnoreCase(operation)) {
                        getFf4j().enable(featureId);
                        res.setContentType(CONTENT_TYPE_HTML);
                        res.getWriter().println(renderMessageBox(msg(featureId, "ENABLED"), "info"));
                        LOGGER.info(featureId + " has been successfully enabled");
                        return;
                    } 
                    
                    // As no return the page is draw
                    if (OP_RMV_FEATURE.equalsIgnoreCase(operation)) {
                        getFf4j().getFeatureStore().delete(featureId);
                        message = msg(featureId, "DELETED");
                        LOGGER.info(featureId + " has been deleted");
                    }
                }
            }

        } catch (Exception e) {
            // Any Error is trapped and display in the console
            messagetype = "error";
            message = e.getMessage();
        }

        // Default page rendering (table)
        renderPage(getFf4j(), req, res, message, messagetype);
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
    private String msg(String featureName, String operationId) {
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
    private String msgGroup(String groupName, String operationId) {
        return String.format("Group <b>%s</b> has been successfully %s", groupName, operationId);
    }

    /** {@inheritDoc} */
    @Override
    public void doPost(HttpServletRequest req, HttpServletResponse res) throws ServletException, IOException {
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
                            ConsoleOperations.importFile(getFf4j(), item.getInputStream());
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
                        ConsoleOperations.updateFeatureDescription(getFf4j(), req);
                        message = msg(req.getParameter(FEATID), "UPDATED");

                    } else if (OP_CREATE_FEATURE.equalsIgnoreCase(operation)) {
                        ConsoleOperations.createFeature(getFf4j(), req);
                        message = msg(req.getParameter(FEATID), "ADDED");

                    } else if (OP_TOGGLE_GROUP.equalsIgnoreCase(operation)) {
                        String groupName = req.getParameter(GROUPNAME);
                        if (groupName != null && !groupName.isEmpty()) {
                            String operationGroup = req.getParameter(SUBOPERATION);
                            if (OP_ENABLE.equalsIgnoreCase(operationGroup)) {
                                getFf4j().getFeatureStore().enableGroup(groupName);
                                message = msgGroup(groupName, "ENABLED");
                                LOGGER.info("Group '" + groupName + "' has been ENABLED.");
                            } else if (OP_DISABLE.equalsIgnoreCase(operationGroup)) {
                                getFf4j().getFeatureStore().disableGroup(groupName);
                                message = msgGroup(groupName, "DISABLED");
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
        renderPage(ff4j, req, res, message, messagetype);
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
