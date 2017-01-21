package org.ff4j.web.controller;

import static org.ff4j.web.bean.WebConstants.NEW_NAME;
import static org.ff4j.web.bean.WebConstants.OP_ADD_FIXEDVALUE;
import static org.ff4j.web.bean.WebConstants.OP_COPY_PROPERTY;
import static org.ff4j.web.bean.WebConstants.OP_CREATE_PROPERTY;
import static org.ff4j.web.bean.WebConstants.OP_DELETE_FIXEDVALUE;
import static org.ff4j.web.bean.WebConstants.OP_EDIT_PROPERTY;
import static org.ff4j.web.bean.WebConstants.OP_RENAME_PROPERTY;
import static org.ff4j.web.bean.WebConstants.OP_RMV_PROPERTY;
import static org.ff4j.web.bean.WebConstants.PARAM_FIXEDVALUE;

/*
 * #%L
 * ff4j-sample-web
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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


import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ff4j.FF4j;
import org.ff4j.property.Property;
import org.ff4j.property.util.PropertyFactory;
import org.ff4j.utils.Util;
import org.ff4j.web.bean.WebConstants;
import org.ff4j.web.embedded.ConsoleOperations;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.thymeleaf.TemplateEngine;
import org.thymeleaf.context.WebContext;

/**
 * Controller for main class
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertiesController extends AbstractController {
    
    /** Logger for this class. */
    public static final Logger LOGGER = LoggerFactory.getLogger(PropertiesController.class);
    
	/** View name. */
	private static final String VIEW_PROPERTIES = "properties";
	
	/** {@inheritDoc} */
	public PropertiesController(FF4j ff4j, TemplateEngine te) {
		super(ff4j, VIEW_PROPERTIES, te);
	}

	/** {@inheritDoc} */
    public void post(HttpServletRequest req, HttpServletResponse res, WebContext ctx)
    throws IOException {
        String msg          = null;
        String msgType      = "success";
        String operation    = req.getParameter(WebConstants.OPERATION);
        String propertyName = req.getParameter(WebConstants.NAME);
        String featureId    = req.getParameter(WebConstants.FEATURE_UID);
        
        if (OP_CREATE_PROPERTY.equalsIgnoreCase(operation)) {
            ConsoleOperations.createProperty(getFf4j(), req);
            msg = propertyName + " has been CREATED";
            String logMessage = "Property '" + propertyName + "' has been created ";
            if (Util.hasLength(featureId)) {
                logMessage+= " for feature '" + featureId + "'";
            }
            LOGGER.info(logMessage);
        } else  if (OP_RMV_PROPERTY.equalsIgnoreCase(operation)) {
            if (!Util.hasLength(propertyName)) {
                msgType = "warning";
                msg = "Property name not found";
            } else {
                getFf4j().getPropertiesStore().deleteProperty(propertyName);
                msg = propertyName + " has been DELETED";
            }
        } else if (OP_EDIT_PROPERTY.equalsIgnoreCase(operation)) {
            ConsoleOperations.updateProperty(getFf4j(), req);
            msg = propertyName + " has been UPDATED";
            String logMessage = "Property '" + propertyName + "' has been UPDATED ";
            if (Util.hasLength(featureId)) {
                logMessage+= " for feature '" + featureId + "'";
            }
            LOGGER.info(logMessage);
            
        } else if (OP_RENAME_PROPERTY.equalsIgnoreCase(operation)) {
            String newName = req.getParameter(NEW_NAME);
            Set< String> propertiesNames = getFf4j().getPropertiesStore().listPropertyNames();
            if (propertiesNames.contains(newName)) {
                msgType = "warning";
                msg = "Cannot rename " + propertyName + " to " + newName + " : it already exists";
            } else {
                Property<?> newProperty = getFf4j().getPropertiesStore().readProperty(propertyName);
                newProperty.setName(newName);
                getFf4j().getPropertiesStore().deleteProperty(propertyName);
                getFf4j().getPropertiesStore().createProperty(newProperty);
                msg = "Property " + propertyName + " has been renamed to " + newName;
            }
            
        } else if (OP_COPY_PROPERTY.equalsIgnoreCase(operation)) {
            String newName = req.getParameter(NEW_NAME);
            Set< String> propertiesNames = getFf4j().getPropertiesStore().listPropertyNames();
            if (propertiesNames.contains(newName)) {
                msgType = "warning";
                msg = "Cannot copy " + propertyName + " to " + newName + " : it already exists";
            } else {
                Property<?> p = getFf4j().getPropertiesStore().readProperty(propertyName);
                Property<?> newProperty = PropertyFactory.createProperty(newName, p.getType(), p.asString(), p.getDescription(), null);
                for(Object o : p.getFixedValues()) {
                    newProperty.add2FixedValueFromString(o.toString());
                }
                getFf4j().getPropertiesStore().createProperty(newProperty);
                msg = "Property " + propertyName + " has been copied to " + newName;
            }
        }
        
        ctx.setVariable("msgType", msgType);
        ctx.setVariable("msgInfo", msg);
        renderPage(ctx);
    }
    
    /** {@inheritDoc} */
    public void get(HttpServletRequest req, HttpServletResponse res, WebContext ctx)
	throws IOException {
        String operation    = req.getParameter(WebConstants.OPERATION);
        String propertyName = req.getParameter(WebConstants.FEATID);

        String msgType = "success";
        String msg = null;
        if (Util.hasLength(operation) && Util.hasLength(propertyName) && 
                getFf4j().getPropertiesStore().existProperty(propertyName)) {
           
            if (OP_DELETE_FIXEDVALUE.equalsIgnoreCase(operation)) {
                String fixedValue = req.getParameter(PARAM_FIXEDVALUE);
                Property<?> ap = getFf4j().getPropertiesStore().readProperty(propertyName);
                // Need to convert back to object to use the remove()
                ap.getFixedValues().remove(ap.fromString(fixedValue));
                getFf4j().getPropertiesStore().updateProperty(ap);
                LOGGER.info("Property '" + propertyName + "' remove fixedValue '" + fixedValue + "'");
            }

            if (OP_ADD_FIXEDVALUE.equalsIgnoreCase(operation)) {
                String fixedValue = req.getParameter(PARAM_FIXEDVALUE);
                Property<?> ap = getFf4j().getPropertiesStore().readProperty(propertyName);
                ap.add2FixedValueFromString(fixedValue);
                getFf4j().getPropertiesStore().updateProperty(ap);
                LOGGER.info("Property '" + propertyName + "' add fixedValue '" + fixedValue + "'");
            }
        }
        ctx.setVariable("msgType", msgType);
        ctx.setVariable("msgInfo", msg);
		renderPage(ctx);
	}

    /**
     * Both get and post operation will render the page.
     *
     * @param ctx
     *            current web context
     */
    private void renderPage(WebContext ctx) {
        ctx.setVariable(KEY_TITLE, "Properties");

        // Sort natural Order
        Map<String, Property<?>> mapOfProperties = ff4j.getPropertiesStore().readAllProperties();
        List<String> propertyNames = Arrays.asList(mapOfProperties.keySet().toArray(new String[0]));
        Collections.sort(propertyNames);
        List<Property<?>> orderedProperties = new ArrayList<Property<?>>();
        for (String propName : propertyNames) {
            orderedProperties.add(mapOfProperties.get(propName));
        }
        ctx.setVariable("listOfProperties", orderedProperties);
    }
    
    

}
