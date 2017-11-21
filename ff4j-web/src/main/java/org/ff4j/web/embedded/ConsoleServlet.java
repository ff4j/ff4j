package org.ff4j.web.embedded;

import static org.ff4j.web.embedded.ConsoleConstants.CONTENT_TYPE_HTML;
import static org.ff4j.web.embedded.ConsoleConstants.CONTENT_TYPE_JSON;
import static org.ff4j.web.embedded.ConsoleConstants.FEATID;
import static org.ff4j.web.embedded.ConsoleConstants.FF4J_SESSIONATTRIBUTE_NAME;
import static org.ff4j.web.embedded.ConsoleConstants.FLIPFILE;
import static org.ff4j.web.embedded.ConsoleConstants.GROUPNAME;
import static org.ff4j.web.embedded.ConsoleConstants.NAME;
import static org.ff4j.web.embedded.ConsoleConstants.OPERATION;
import static org.ff4j.web.embedded.ConsoleConstants.OP_ADD_FIXEDVALUE;
import static org.ff4j.web.embedded.ConsoleConstants.OP_CREATE_FEATURE;
import static org.ff4j.web.embedded.ConsoleConstants.OP_CREATE_PROPERTY;
import static org.ff4j.web.embedded.ConsoleConstants.OP_DELETE_FIXEDVALUE;
import static org.ff4j.web.embedded.ConsoleConstants.OP_DISABLE;
import static org.ff4j.web.embedded.ConsoleConstants.OP_EDIT_FEATURE;
import static org.ff4j.web.embedded.ConsoleConstants.OP_EDIT_PROPERTY;
import static org.ff4j.web.embedded.ConsoleConstants.OP_ENABLE;
import static org.ff4j.web.embedded.ConsoleConstants.OP_EXPORT;
import static org.ff4j.web.embedded.ConsoleConstants.OP_READ_FEATURE;
import static org.ff4j.web.embedded.ConsoleConstants.OP_READ_PROPERTY;
import static org.ff4j.web.embedded.ConsoleConstants.OP_RMV_FEATURE;
import static org.ff4j.web.embedded.ConsoleConstants.OP_RMV_PROPERTY;
import static org.ff4j.web.embedded.ConsoleConstants.OP_TOGGLE_GROUP;
import static org.ff4j.web.embedded.ConsoleConstants.PARAM_FIXEDVALUE;
import static org.ff4j.web.embedded.ConsoleConstants.PROVIDER_PARAM_NAME;
import static org.ff4j.web.embedded.ConsoleConstants.SUBOPERATION;
import static org.ff4j.web.embedded.ConsoleConstants.VIEW;
import static org.ff4j.web.embedded.ConsoleOperations.createFeature;
import static org.ff4j.web.embedded.ConsoleOperations.createProperty;
import static org.ff4j.web.embedded.ConsoleOperations.exportFile;
import static org.ff4j.web.embedded.ConsoleOperations.importFile;
import static org.ff4j.web.embedded.ConsoleOperations.updateFeatureDescription;
import static org.ff4j.web.embedded.ConsoleOperations.updateProperty;
import static org.ff4j.web.embedded.ConsoleRenderer.msg;
import static org.ff4j.web.embedded.ConsoleRenderer.renderMessageBox;
import static org.ff4j.web.embedded.ConsoleRenderer.renderMsgGroup;
import static org.ff4j.web.embedded.ConsoleRenderer.renderMsgProperty;
import static org.ff4j.web.embedded.ConsoleRenderer.renderPage;
import static org.ff4j.web.embedded.ConsoleRenderer.renderPageMonitoring;

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
import org.ff4j.core.Feature;
import org.ff4j.property.Property;
import org.ff4j.web.FF4jProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Unique Servlet to manage FlipPoints and security
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class ConsoleServlet extends HttpServlet {

    /** serial number. */
    private static final long serialVersionUID = -3982043895954284269L;

    /** Logger for this class. */
    public static final Logger LOGGER = LoggerFactory.getLogger(ConsoleServlet.class);
    
    /** Error Message. */
    public static final String ERROR = "error";

    /** instance of ff4j. */
    private FF4j ff4j = null;

    /** initializing ff4j provider. */
    private FF4jProvider ff4jProvider = null;

    /**
     * Servlet initialization, init FF4J from "ff4jProvider" attribute Name.
     *
     * @param servletConfig
     *            current {@link ServletConfig} context
     * @throws ServletException
     *             error during servlet initialization
     */
    public void init(ServletConfig servletConfig) throws ServletException {
        LOGGER.info("  __  __ _  _   _ ");
        LOGGER.info(" / _|/ _| || | (_)");
        LOGGER.info("| |_| |_| || |_| |");
        LOGGER.info("|  _|  _|__   _| |");
        LOGGER.info("|_| |_|    |_|_/ |");
        LOGGER.info("             |__/   Embedded Console - v" + getClass().getPackage().getImplementationVersion());
        LOGGER.info(" ");
        
        if (ff4j == null) {
            String className = servletConfig.getInitParameter(PROVIDER_PARAM_NAME);
            try {
                Class<?> c = Class.forName(className);
                Object o = c.newInstance();
                ff4jProvider = (FF4jProvider) o;
                LOGGER.info("ff4j context has been successfully initialized - {} feature(s)", ff4jProvider.getFF4j().getFeatures().size());
            } catch (ClassNotFoundException e) {
                throw new IllegalArgumentException("Cannot load ff4jProvider as " + ff4jProvider, e);
            } catch (InstantiationException e) {
                throw new IllegalArgumentException("Cannot instantiate  " + ff4jProvider + " as ff4jProvider", e);
            } catch (IllegalAccessException e) {
                throw new IllegalArgumentException("No public constructor for  " + ff4jProvider + " as ff4jProvider", e);
            } catch (ClassCastException ce) {
                throw new IllegalArgumentException("ff4jProvider expected instance of " + FF4jProvider.class, ce);
            }

            // Put the FF4J in ApplicationScope (useful for tags)
            ff4j = ff4jProvider.getFF4j();
            servletConfig.getServletContext().setAttribute(FF4J_SESSIONATTRIBUTE_NAME, ff4j);
            LOGGER.debug("Servlet has been initialized and ff4j store in session with {} ", ff4j.getFeatures().size());
        } else {
            LOGGER.debug("Servlet has been initialized, ff4j was injected");
        }
    }

    /** {@inheritDoc} */
    public void doGet(HttpServletRequest req, HttpServletResponse res)
    throws ServletException, IOException {
    	String targetView = req.getParameter(VIEW);
    	if (targetView == null || "".equals(targetView)) {
    		pageCore(req, res);
    	} else if ("monitoring".equals(targetView)) {
    		pageMonitoring(req, res);
    	}
    }
    
    public void pageMonitoring(HttpServletRequest req, HttpServletResponse res) throws IOException {
    	String message = null;
        String messagetype = "info";
        renderPageMonitoring(getFf4j(), req, res, message, messagetype);
    }
    
    public void pageCore(HttpServletRequest req, HttpServletResponse res) throws IOException {
    	String message = null;
        String messagetype = "info";
        try {
            
            // 'RSC' parameter will load some static resources
            if (ConsoleRenderer.renderResources(req, res)) return;

            // Serve operation from GET
            String operation = req.getParameter(OPERATION);
            String featureId = req.getParameter(FEATID);
            LOGGER.info("GET - op=" + operation + " feat=" + featureId);
            if (operation != null && !operation.isEmpty()) {
                
                // operation which do not required features
                if (OP_EXPORT.equalsIgnoreCase(operation)) {
                    exportFile(ff4j, res);
                    return;
                }
                
                // Work on a feature ID
                if ((featureId != null) && (!featureId.isEmpty())) {
                    if (getFf4j().getFeatureStore().exist(featureId)) {
                        message = workWithFeature(res, operation, featureId);
                    }
                    if (getFf4j().getPropertiesStore().existProperty(featureId)) {
                        message = workWithProperties(res, req, operation, featureId);
                    }
                }
            }

        } catch (Exception e) {
            // Any Error is trapped and display in the console
            messagetype = ERROR;
            message = e.getMessage();
            LOGGER.error("An error occured ", e);
        }

        // Default page rendering (table)
        renderPage(getFf4j(), req, res, message, messagetype);
    }
    
    /**
     * Execute operation on properties when needed.
     *
     * @param res
     *      target response
     * @param req
     *      current request
     * @param operation
     *      operation to be completed on properties
     * @param uid
     *      unique property identifier
     * @return
     *      targte message to be displayed
     * @throws IOException
     *      error occurs when filling the output page
     */
    private String workWithProperties(HttpServletResponse res, HttpServletRequest req, String operation, String uid) 
    throws IOException {
        String message = null;
        if (OP_RMV_PROPERTY.equalsIgnoreCase(operation)) {
            getFf4j().getPropertiesStore().deleteProperty(uid);
            LOGGER.info("Property '" + uid + "' has been deleted");
            message = renderMsgProperty(uid, "DELETED");
        }
        
        if (OP_READ_PROPERTY.equalsIgnoreCase(operation)) {
            Property<?> ap = getFf4j().getPropertiesStore().readProperty(uid);
            res.setContentType(CONTENT_TYPE_JSON);
            res.getWriter().println(ap.toString());
        }
        
        if (OP_DELETE_FIXEDVALUE.equalsIgnoreCase(operation)) {
            String fixedValue = req.getParameter(PARAM_FIXEDVALUE);
            Property<?> ap = getFf4j().getPropertiesStore().readProperty(uid);
            ap.getFixedValues().remove(fixedValue);
            getFf4j().getPropertiesStore().updateProperty(ap);
        }
        
        if (OP_ADD_FIXEDVALUE.equalsIgnoreCase(operation)) {
            String fixedValue = req.getParameter(PARAM_FIXEDVALUE);
            Property<?> ap = getFf4j().getPropertiesStore().readProperty(uid);
            ap.add2FixedValueFromString(fixedValue);
            getFf4j().getPropertiesStore().updateProperty(ap);
        }
        return message;
    }
    
    /**
     * Operations relative to features.
     *
     * @param res
     *      http response
     * @param operation
     *      operation to execute on features
     * @param uid
     *      unique feature identifier
     * @throws IOException
     *      an error occur when fill the output
     */
    private String workWithFeature(HttpServletResponse res, String operation, String uid)
    throws IOException {
        String message = null;
        if (OP_DISABLE.equalsIgnoreCase(operation)) {
            getFf4j().getFeatureStore().disable(uid);
            res.setContentType(CONTENT_TYPE_HTML);
            res.getWriter().println(renderMessageBox(msg(uid, "DISABLED"), "info"));
            LOGGER.info(uid + " has been disabled");
        } 
        
        if (OP_ENABLE.equalsIgnoreCase(operation)) {
            getFf4j().getFeatureStore().enable(uid);
            res.setContentType(CONTENT_TYPE_HTML);
            res.getWriter().println(renderMessageBox(msg(uid, "ENABLED"), "info"));
            LOGGER.info("Feature '" + uid + "' has been successfully enabled");
        }
        
        if (OP_READ_FEATURE.equalsIgnoreCase(operation)) {
            Feature f = getFf4j().getFeatureStore().read(uid);
            res.setContentType(CONTENT_TYPE_JSON);
            res.getWriter().println(f.toJson());
        }
        
        // As no return the page is draw
        if (OP_RMV_FEATURE.equalsIgnoreCase(operation)) {
            getFf4j().getFeatureStore().delete(uid);
            LOGGER.info(uid + " has been deleted");
            message = msg(uid, "DELETED");
        }
        return message;
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
                            importFile(getFf4j(), item.getInputStream());
                            message = "The file <b>" + filename + "</b> has been successfully imported";
                        } else {
                            messagetype = ERROR;
                            message = "Invalid FILE, must be CSV, XML or PROPERTIES files";
                        }
                    }
                }

            } else {
                
                String operation = req.getParameter(OPERATION);
                String uid = req.getParameter(FEATID);
                LOGGER.info("POST - op=" + operation + " feat=" + uid);
                if (operation != null && !operation.isEmpty()) {

                    if (OP_EDIT_FEATURE.equalsIgnoreCase(operation)) {
                        updateFeatureDescription(getFf4j(), req);
                        message = msg(uid, "UPDATED");

                    } else if (OP_EDIT_PROPERTY.equalsIgnoreCase(operation)) {
                        updateProperty(getFf4j(), req);
                        message = renderMsgProperty(uid, "UPDATED");
                       
                    } else if (OP_CREATE_PROPERTY.equalsIgnoreCase(operation)) {
                        createProperty(getFf4j(), req);
                        message = renderMsgProperty(req.getParameter(NAME), "ADDED");
                        
                    } else if (OP_CREATE_FEATURE.equalsIgnoreCase(operation)) {
                        createFeature(getFf4j(), req);
                        message = msg(uid, "ADDED");

                    } else if (OP_TOGGLE_GROUP.equalsIgnoreCase(operation)) {
                        String groupName = req.getParameter(GROUPNAME);
                        if (groupName != null && !groupName.isEmpty()) {
                            String operationGroup = req.getParameter(SUBOPERATION);
                            if (OP_ENABLE.equalsIgnoreCase(operationGroup)) {
                                getFf4j().getFeatureStore().enableGroup(groupName);
                                message = renderMsgGroup(groupName, "ENABLED");
                                LOGGER.info("Group '" + groupName + "' has been ENABLED.");
                            } else if (OP_DISABLE.equalsIgnoreCase(operationGroup)) {
                                getFf4j().getFeatureStore().disableGroup(groupName);
                                message = renderMsgGroup(groupName, "DISABLED");
                                LOGGER.info("Group '" + groupName + "' has been DISABLED.");
                            }
                        }
                    } else {
                        LOGGER.error("Invalid POST OPERATION" + operation);
                        messagetype = ERROR;
                        message = "Invalid REQUEST";
                    }
                } else {
                    LOGGER.error("No ID provided" + operation);
                    messagetype = ERROR;
                    message = "Invalid UID";
                }
            }

        } catch (Exception e) {
            messagetype = ERROR;
            message = e.getMessage();
            LOGGER.error("An error occured ", e);
        }
        // Update FF4J in Session (distributed)
        getServletContext().setAttribute(FF4J_SESSIONATTRIBUTE_NAME, ff4j);
        
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

    /**
     * Setter accessor for attribute 'ff4j'.
     * @param ff4j
     * 		new value for 'ff4j '
     */
    public void setFf4j(FF4j ff4j) {
        this.ff4j = ff4j;
    }

}