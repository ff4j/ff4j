package org.ff4j.web.embedded;

import static org.ff4j.web.embedded.ConsoleConstants.CONTENT_TYPE_HTML;
import static org.ff4j.web.embedded.ConsoleConstants.CONTENT_TYPE_JSON;
import static org.ff4j.web.embedded.ConsoleConstants.FEATID;
import static org.ff4j.web.embedded.ConsoleConstants.FLIPFILE;
import static org.ff4j.web.embedded.ConsoleConstants.VIEW;
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
import static org.ff4j.web.embedded.ConsoleConstants.SUBOPERATION;
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

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.apache.commons.io.FilenameUtils;
import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.property.Property;
import org.ff4j.web.FF4jInitServlet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Unique Servlet to manage FlipPoints and security
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class ConsoleServlet extends FF4jInitServlet {

    /** serial number. */
    private static final long serialVersionUID = -3982043895954284269L;

    /** Logger for this class. */
    public static final Logger LOGGER = LoggerFactory.getLogger(ConsoleServlet.class);
    
    /** Error Message. */
    public static final String ERROR = "error";

    /** {@inheritDoc} */
    public void doGet(HttpServletRequest req, HttpServletResponse res)
    throws ServletException, IOException {
    	
    	if (ConsoleRenderer.renderResources(req, res)) return;
    	 
    	/* Sample Context
    	ServletContext sc = req.getSession().getServletContext();
    	WebContext ctx = new WebContext(req, res, sc, req.getLocale());
    	ctx.setVariable("CHAINE", "Sample Valeur");
    	ApiConfig api = new ApiConfig();
    	api.setHost("HOST_X");
    	ctx.setVariable("BEAN", api);
    	ctx.setVariable("today", Calendar.getInstance());
    	
    	List < ApiConfig > listA = new ArrayList<ApiConfig>();
    	listA.add(new ApiConfig());
    	listA.add(new ApiConfig());
    	listA.add(api);
    	ctx.setVariable("apis", listA);
    	
    	templateEngine.process("home", ctx, res.getWriter());
	    */
    	/*
    	if (ff4j.check("ff4j.admin.secure")) {
    		 	PropertyStringList listOfUsers = (PropertyStringList) 
    				ff4j.getPropertiesStore().readProperty("authorizedUsers");
    		if (!listOfUsers.contains(ff4j.getAuthorizationsManager().getCurrentUserName())) {
    			// Forbidden
    		}
    	}*/
    	
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
        try {
        	
        	
        	
        } catch (Exception e) {
            // Any Error is trapped and display in the console
            messagetype = ERROR;
            message = e.getMessage();
            LOGGER.error("An error occured ", e);
        }
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
                            LOGGER.info("Feature '" + featureId + "' has been successfully enabled");
                            return;
                        }
                        
                        if (OP_READ_FEATURE.equalsIgnoreCase(operation)) {
                            Feature f = getFf4j().getFeatureStore().read(featureId);
                            res.setContentType(CONTENT_TYPE_JSON);
                            res.getWriter().println(f.toJson());
                            return;
                        }
                        
                        // As no return the page is draw
                        if (OP_RMV_FEATURE.equalsIgnoreCase(operation)) {
                            getFf4j().getFeatureStore().delete(featureId);
                            message = msg(featureId, "DELETED");
                            LOGGER.info(featureId + " has been deleted");
                        }
                        
                    }
                    
                    if (getFf4j().getPropertiesStore().existProperty(featureId)) {
                    
                        if (OP_RMV_PROPERTY.equalsIgnoreCase(operation)) {
                            getFf4j().getPropertiesStore().deleteProperty(featureId);
                            message = renderMsgProperty(featureId, "DELETED");
                            LOGGER.info("Property '" + featureId + "' has been deleted");
                        }
                        
                        if (OP_READ_PROPERTY.equalsIgnoreCase(operation)) {
                            Property<?> ap = getFf4j().getPropertiesStore().readProperty(featureId);
                            res.setContentType(CONTENT_TYPE_JSON);
                            res.getWriter().println(ap.toString());
                            return;
                        }
                        
                        if (OP_DELETE_FIXEDVALUE.equalsIgnoreCase(operation)) {
                            String fixedValue = req.getParameter(PARAM_FIXEDVALUE);
                            Property<?> ap = getFf4j().getPropertiesStore().readProperty(featureId);
                            ap.getFixedValues().remove(fixedValue);
                            getFf4j().getPropertiesStore().updateProperty(ap);
                            return;
                        }
                        
                        if (OP_ADD_FIXEDVALUE.equalsIgnoreCase(operation)) {
                            String fixedValue = req.getParameter(PARAM_FIXEDVALUE);
                            Property<?> ap = getFf4j().getPropertiesStore().readProperty(featureId);
                            ap.add2FixedValueFromString(fixedValue);
                            getFf4j().getPropertiesStore().updateProperty(ap);
                            return;
                        }
                        
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
