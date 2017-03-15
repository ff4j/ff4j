package org.ff4j.web.embedded;

/*
 * #%L
 * ff4j-web
 * %%
 * Copyright (C) 2013 - 2015 FF4J
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
import java.io.InputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ff4j.FF4j;
import org.ff4j.conf.XmlConfig;
import org.ff4j.conf.XmlParser;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.property.Property;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.property.util.PropertyFactory;
import org.ff4j.utils.Util;
import org.ff4j.web.bean.WebConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.ff4j.web.embedded.ConsoleConstants.*;

public final class ConsoleOperations {
    
    /** Logger for this class. */
    private static Logger LOGGER = LoggerFactory.getLogger(ConsoleOperations.class);
    
    private ConsoleOperations() {}
    
    /**
     * User action to create a new Feature.
     * 
     * @param req
     *            http request containing operation parameters
     */
    public static void createFeature(FF4j ff4j, HttpServletRequest req) {
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
                    throw new IllegalArgumentException("Cannot instantiate strategy", e);
                } catch (IllegalAccessException e) {
                    throw new IllegalArgumentException("Cannot instantiate : no public constructor", e);
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
            ff4j.getFeatureStore().create(fp);
            LOGGER.info(featureId + " has been created");
        }
    }
    
    /**
     * Sample Element should be updated like name, description, value
     * @param ff4j
     * @param req
     */
    public static void updateProperty(FF4j ff4j, HttpServletRequest req) {
        String name         = req.getParameter("name");
        String type         = req.getParameter("pType");
        String description  = req.getParameter("desc");
        String value        = req.getParameter("pValue");
        String uid          = req.getParameter("uid");
        String featureId    = req.getParameter(WebConstants.FEATURE_UID);
        
        Property<?> ap;
        // To update the core the uid is the name (rename, edit)
        if (uid == null) {
            uid = name;
        }
        
        // Update Feature property
        if (Util.hasLength(featureId)) {
            
            Feature current = ff4j.getFeatureStore().read(featureId);
            ap = current.getProperty(uid);
            ap.setDescription(description);
            if (ap.getType().equalsIgnoreCase(type)) {
                ap.setValueFromString(value);
            } else {
                ap = PropertyFactory.createProperty(name, type, value);
                LOGGER.warn("By changing property type you loose the fixedValues, cannot evaluate ? at runtime");
            }
            ff4j.getFeatureStore().update(current);
             
        } else if (ff4j.getPropertiesStore().existProperty(uid)) {
            
            // Do not change name, just and update
            if (uid.equalsIgnoreCase(name)) {
                ap = ff4j.getPropertiesStore().readProperty(uid);
                // just an update for the value
                if (ap.getType().equalsIgnoreCase(type)) {
                    ap.setDescription(description);
                    ap.setValueFromString(value);
                    ff4j.getPropertiesStore().updateProperty(ap);
                } else {
                    ap = PropertyFactory.createProperty(name, type, value);
                    ap.setDescription(description);
                    // Note : Fixed Values are LOST if type changed => cannot cast ? to T
                    LOGGER.warn("By changing property type you loose the fixedValues, cannot evaluate ? at runtime");
                    ff4j.getPropertiesStore().deleteProperty(name);
                    ff4j.getPropertiesStore().createProperty(ap);
                }
                
            } else {
                // Name change delete and create a new
                ap = PropertyFactory.createProperty(name, type, value);
                ap.setDescription(description);
                // Note : Fixed Values are LOST if name changed => cannot cast ? to T
                LOGGER.warn("By changing property name you loose the fixedValues, cannot evaluate generics at runtime (type inference)");
                ff4j.getPropertiesStore().deleteProperty(uid);
                ff4j.getPropertiesStore().createProperty(ap);
            }
        }
    }
    
    /**
     * Create new property in store.
     *
     * @param ff4j
     *      current ff4j instance.
     * @param req
     *      current http request
     */
    public static void createProperty(FF4j ff4j, HttpServletRequest req) {
        String name         = req.getParameter("name");
        String type         = req.getParameter("pType");
        String description  = req.getParameter("desc");
        String value        = req.getParameter("pValue");
        String featureId    = req.getParameter(WebConstants.FEATURE_UID);
        Property<?> ap = PropertyFactory.createProperty(name, type, value);
        ap.setDescription(description);
        
        if (Util.hasLength(featureId)) {
            Feature current = ff4j.getFeatureStore().read(featureId);
            current.addProperty(ap);
            ff4j.getFeatureStore().update(current);
        } else {
            ff4j.getPropertiesStore().createProperty(ap);
        }
    }

    private static void updateFlippingStrategy(Feature fp, String strategy, String strategyParams) {
        
        if (null != strategy && !strategy.isEmpty()) {
            try {
                Class<?> strategyClass = Class.forName(strategy);
                FlippingStrategy fstrategy = (FlippingStrategy) strategyClass.newInstance();
               
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
                    fstrategy.init(fp.getUid(), initParams);
                }
                fp.setFlippingStrategy(fstrategy);

            } catch (ClassNotFoundException e) {
                throw new IllegalArgumentException("Cannot find strategy class", e);
            } catch (InstantiationException e) {
                throw new IllegalArgumentException("Cannot instantiate strategy", e);
            } catch (IllegalAccessException e) {
                throw new IllegalArgumentException("Cannot instantiate : no public constructor", e);
            }
        }
    }
    
    /**
     * User action to update a target feature's description.
     * 
     * @param req
     *            http request containing operation parameters
     */
    public static void updateFeatureDescription(FF4j ff4j, HttpServletRequest req) {
        // uid
        final String featureId = req.getParameter(FEATID);
        if (featureId != null && !featureId.isEmpty()) {
            // https://github.com/clun/ff4j/issues/66
            Feature old = ff4j.getFeatureStore().read(featureId);
            Feature fp = new Feature(featureId, old.isEnable());
            // <--
            
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
            updateFlippingStrategy(fp, req.getParameter(STRATEGY), req.getParameter(STRATEGY_INIT));

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
            ff4j.getFeatureStore().update(fp);
            LOGGER.info(featureId + " has been updated");
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
    public static void importFile(FF4j ff4j, InputStream in) 
    throws IOException {
        
        FeatureStore store = ff4j.getFeatureStore();
        XmlConfig xmlConfig = new XmlParser().parseConfigurationFile(in);
        Map<String, Feature> mapsOfFeat = xmlConfig.getFeatures();
        for (Entry<String, Feature> feature : mapsOfFeat.entrySet()) {
            if (store.exist(feature.getKey())) {
                store.update(feature.getValue());
            } else {
                store.create(feature.getValue());
            }
        }
        LOGGER.info(mapsOfFeat.size() + " features have been imported.");
        
        PropertyStore pstore = ff4j.getPropertiesStore();
        Map<String, Property<?>> mapsOfProperties = xmlConfig.getProperties();
        for (Entry<String, Property<?>> p : mapsOfProperties.entrySet()) {
            if (pstore.existProperty(p.getKey())) {
                pstore.updateProperty(p.getValue());
            } else {
                pstore.createProperty(p.getValue());
            }
        }
        LOGGER.info(mapsOfProperties.size() + " features have been imported.");
    }
    
    /**
     * Build Http response when invoking export features.
     * 
     * @param res
     *            http response
     * @throws IOException
     *             error when building response
     */
    public static void exportFile(FF4j ff4j, HttpServletResponse res) throws IOException {
        Map<String, Feature> features = ff4j.getFeatureStore().readAll();
        InputStream in = new XmlParser().exportFeatures(features);
        ServletOutputStream sos = null;
        try {
            sos = res.getOutputStream();
            res.setContentType("text/xml");
            res.setHeader("Content-Disposition", "attachment; filename=\"ff4j.xml\"");
            // res.setContentLength()
            org.apache.commons.io.IOUtils.copy(in, sos);
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
}
