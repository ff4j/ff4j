package org.ff4j.web.controller;

/*-
 * #%L
 * ff4j-web
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.utils.MappingUtil;
import org.ff4j.utils.Util;
import org.ff4j.web.bean.WebConstants;
import org.ff4j.web.embedded.ConsoleOperations;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.thymeleaf.TemplateEngine;
import org.thymeleaf.context.WebContext;

import java.io.IOException;
import java.util.*;

import static org.ff4j.web.bean.WebConstants.*;

/**
 * Controller for main class
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class FeaturesController extends AbstractController {

	/** Logger for this class. */
    public static final Logger LOGGER = LoggerFactory.getLogger(FeaturesController.class);

    /** View name. */
	private static final String VIEW_FEATURES = "features";

	/** {@inheritDoc} */
	public FeaturesController(FF4j ff4j, TemplateEngine te) {
		super(ff4j, VIEW_FEATURES, te);
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

    /** {@inheritDoc} */
    public void get(HttpServletRequest req, HttpServletResponse res, WebContext ctx) throws IOException {
        String operation = req.getParameter(WebConstants.OPERATION);
        String featureId = req.getParameter(WebConstants.FEATID);

        String msgType = "success";
        String msg = null;
        if (Util.hasLength(operation) && Util.hasLength(featureId)) {
            if (getFf4j().getFeatureStore().exist(featureId)) {
                
                if (OP_DISABLE.equalsIgnoreCase(operation)) {
                    getFf4j().disable(featureId);
                    msg = msg(featureId, "DISABLED");
                    LOGGER.info(featureId + " has been disabled");
                }
                
                if (OP_ENABLE.equalsIgnoreCase(operation)) {
                    getFf4j().enable(featureId);
                    msg = msg(featureId, "ENABLED");
                    LOGGER.info(featureId + " has been enabled");
                }
                
                if (OP_ADD_PERMISSION.equalsIgnoreCase(operation)) {
                    String permName = req.getParameter(WebConstants.PERMISSION);
                    getFf4j().getFeatureStore().grantRoleOnFeature(featureId, permName);
                    LOGGER.info("Add new " + permName + " to " + featureId );
                }
                
                if (OP_RMV_PERMISSION.equalsIgnoreCase(operation)) {
                    String permName = req.getParameter(WebConstants.PERMISSION);
                    getFf4j().getFeatureStore().removeRoleFromFeature(featureId, permName);
                    LOGGER.info("Remove " + permName + " to " + featureId );
                }
                
                if (OP_CLEAR_PERMISSIONS.equalsIgnoreCase(operation)) {
                    Feature feature = getFf4j().getFeatureStore().read(featureId);
                    feature.getPermissions().clear();
                    getFf4j().getFeatureStore().update(feature);
                    LOGGER.info("Clear permissions for " + featureId);
                }
                
                if (OP_RMV_PROPERTY.equalsIgnoreCase(operation)) {
                    String propertyName = req.getParameter(WebConstants.NAME);
                    Feature feature     = getFf4j().getFeatureStore().read(featureId);
                    feature.getCustomProperties().remove(propertyName);
                    getFf4j().getFeatureStore().update(feature);
                    LOGGER.info("Remove Property " + propertyName + " to " + featureId );
                }
                    
            } else {
                msgType = "warning";
                msg = "The feature '" + featureId + "' does not exist";
            }
        }
        ctx.setVariable("msgType", msgType);
        ctx.setVariable("msgInfo", msg);
        renderPage(ctx);
    }

	/** {@inheritDoc} */
    public void post(HttpServletRequest req, HttpServletResponse res, WebContext ctx)
    throws IOException {
        String msg       = null;
        String msgType   = "success";
        String operation = req.getParameter(WebConstants.OPERATION);
        String featureId = req.getParameter(WebConstants.FEATID);
        
        if (OP_EDIT_FEATURE.equalsIgnoreCase(operation)) {
            this.updateFeature(req, featureId);
            msg = featureId + " has been UPDATED";
            
        } else if (OP_CREATE_FEATURE.equalsIgnoreCase(operation)) {
            ConsoleOperations.createFeature(getFf4j(), req);
            msg = featureId + " has been CREATED";
            
        } else if (OP_RMV_FEATURE.equalsIgnoreCase(operation)) {
            getFf4j().getFeatureStore().delete(featureId);
            msg = featureId + " has been DELETED";
            
        } else if (OP_RENAME_FEATURE.equalsIgnoreCase(operation)) {
            String newName = req.getParameter(NEW_NAME);
            Set< String> featureNames = getFf4j().getFeatureStore().readAll().keySet();
            if (featureNames.contains(newName)) {
                msgType = "warning";
                msg = "Cannot rename " + featureId + " to " + newName + " : it already exists";
            } else {
                Feature newFeature = getFf4j().getFeatureStore().read(featureId);
                newFeature.setUid(newName);
                getFf4j().getFeatureStore().delete(featureId);
                getFf4j().getFeatureStore().create(newFeature);
                msg = "Feature " + featureId + " has been renamed to " + newName;
            }
            
        } else if (OP_COPY_FEATURE.equalsIgnoreCase(operation)) {
            String newName = req.getParameter(NEW_NAME);
            Set< String> featureNames = getFf4j().getFeatureStore().readAll().keySet();
            if (featureNames.contains(newName)) {
                msgType = "warning";
                msg = "Cannot copy " + featureId + " with name " + newName + " : it already exists";
            } else {
                Feature newFeature = new Feature(getFf4j().getFeatureStore().read(featureId));
                newFeature.setUid(newName);
                getFf4j().getFeatureStore().create(newFeature);
                msg = "Feature " + featureId + " has been copied to " + newName;
            }
            
        } else if (OP_TOGGLE_GROUP.equalsIgnoreCase(operation)) {
            String groupName = req.getParameter(GROUPNAME);
            if (groupName != null && !groupName.isEmpty()) {
                String operationGroup = req.getParameter(SUBOPERATION);
                if (OP_ENABLE.equalsIgnoreCase(operationGroup)) {
                    getFf4j().getFeatureStore().enableGroup(groupName);
                    msg = groupName + " has been ENABLED";
                    LOGGER.info("Group '" + groupName + "' has been ENABLED.");
                } else if (OP_DISABLE.equalsIgnoreCase(operationGroup)) {
                    getFf4j().getFeatureStore().disableGroup(groupName);
                    msg = groupName + " has been DISABLED";
                    LOGGER.info("Group '" + groupName + "' has been DISABLED.");
                }
            }
        }
        
        ctx.setVariable("msgType", msgType);
        ctx.setVariable("msgInfo", msg);
        renderPage(ctx);
    }
    
    /**
     * Allow to update feature.
     *
     * @param featureId
     */
    private void updateFeature(HttpServletRequest req, String featureId) {
        Feature old = ff4j.getFeatureStore().read(featureId);
        // Core
        Feature fp = new Feature(featureId, old.isEnable());
        fp.setPermissions(old.getPermissions());
        fp.setCustomProperties(old.getCustomProperties());
        fp.setFlippingStrategy(buildFlippingStrategy(req, fp.getUid()));
        
        // Description
        final String featureDesc = req.getParameter(DESCRIPTION);
        if (Util.hasLength(featureDesc)) {
            fp.setDescription(featureDesc);
        }
        // GroupName
        final String groupName = req.getParameter(GROUPNAME);
        if (Util.hasLength(groupName)) {
            fp.setGroup(groupName);
        }
        // Creation
        ff4j.getFeatureStore().update(fp);
    }
  
    /**
     * Create Flipping Strategy from parameters.
     *
     * @param req
     *            current http query
     * @param uid
     *      unique feature identifier
     * @return instance of strategy
     */
    private FlippingStrategy buildFlippingStrategy(HttpServletRequest req, String uid) {
        String strategy = req.getParameter(STRATEGY);
        String strategyParams = req.getParameter(STRATEGY_INIT);
        // --> Escaping Special Character to be able to SPLIT
        
        // <--
        FlippingStrategy fstrategy = null;
        Map<String, String> initParams = new HashMap<String, String>();
        if (Util.hasLength(strategy)) {
            if (Util.hasLength(strategyParams)) {
                String[] params = strategyParams.split(";");
                for (String currentP : params) {
                    String[] cur = currentP.split("=");
                    String value = (cur.length < 2) ? "" : cur[1];
                    initParams.put(cur[0], value);
                }
            }
            fstrategy = MappingUtil.instanceFlippingStrategy(uid, strategy, initParams);
        }
        return fstrategy;
    }

    /**
     * Both get and post operation will render the page.
     *
     * @param ctx
     *            current web context
     */
    private void renderPage(WebContext ctx) {
        ctx.setVariable(KEY_TITLE, "Features");

        // Sort natural Order
        Map<String, Feature> mapOfFeatures = ff4j.getFeatureStore().readAll();
        List<String> featuresNames = Arrays.asList(mapOfFeatures.keySet().toArray(new String[0]));
        Collections.sort(featuresNames);
        List<Feature> orderedFeatures = new ArrayList<Feature>();
        List<FlippingStrategy> orderedStrategyUsed = new ArrayList<FlippingStrategy>();
        Map<String,FlippingStrategy> mapOfStrategyUsed = new HashMap<String, FlippingStrategy>();

        for (String featuName : featuresNames) {
            Feature feature = mapOfFeatures.get(featuName);
            orderedFeatures.add(feature);
            FlippingStrategy strategyTargered = feature.getFlippingStrategy();
            if (strategyTargered!=null) {
                if (mapOfStrategyUsed.get(strategyTargered.getClass().getSimpleName()) != null) {
                } else {
                    mapOfStrategyUsed.put(strategyTargered.getClass().getSimpleName(), strategyTargered);
                    orderedStrategyUsed.add(strategyTargered);
                }
            }
        }

        ctx.setVariable("listOfFeatures", orderedFeatures);

        ctx.setVariable("listOfStrategyUsed", orderedStrategyUsed);


        // Get Group List
        List<String> myGroupList = new ArrayList<String>(ff4j.getFeatureStore().readAllGroups());
        Collections.sort(myGroupList);
        ctx.setVariable("groupList", myGroupList);
    }

}
