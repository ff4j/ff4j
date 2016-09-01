package org.ff4j.web.controller;

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

import static org.ff4j.web.bean.WebConstants.CONTENT_TYPE_JSON;
import static org.ff4j.web.bean.WebConstants.GRAPH_BAR_HITRATIO;
import static org.ff4j.web.bean.WebConstants.GRAPH_PIE_HITRATIO;
import static org.ff4j.web.bean.WebConstants.GRAPH_PIE_HOST;
import static org.ff4j.web.bean.WebConstants.GRAPH_PIE_SOURCE;
import static org.ff4j.web.bean.WebConstants.GRAPH_PIE_USER;
import static org.ff4j.web.bean.WebConstants.OP_AUDIT;
import static org.ff4j.web.bean.WebConstants.OP_EXPORT;
import static org.ff4j.web.bean.WebConstants.OP_FEATURES;
import static org.ff4j.web.bean.WebConstants.OP_FEATUREUSAGE;
import static org.ff4j.web.bean.WebConstants.OP_PROPERTIES;
import static org.ff4j.web.bean.WebConstants.OP_TIMESERIES;
import static org.ff4j.web.embedded.ConsoleOperations.exportFile;

import java.io.IOException;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ff4j.FF4j;
import org.ff4j.audit.Event;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.chart.BarChart;
import org.ff4j.audit.chart.PieChart;
import org.ff4j.audit.chart.TimeSeriesChart;
import org.ff4j.core.Feature;
import org.ff4j.property.Property;
import org.ff4j.web.bean.WebConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.thymeleaf.TemplateEngine;
import org.thymeleaf.context.WebContext;

/**
 * Mini API to get informations through AJAX in JSON.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class OperationsController extends AbstractController {
    
    /** Logger for this class. */
    public static final Logger LOGGER = LoggerFactory.getLogger(OperationsController.class);

    /** {@inheritDoc} */
    public OperationsController(FF4j ff4j, TemplateEngine te) {
        super(ff4j, null, te);
    }

    /** {@inheritDoc} */
    public void post(HttpServletRequest req, HttpServletResponse res, WebContext ctx)
    throws IOException {
    }
    
    /** {@inheritDoc} */
    public void get(HttpServletRequest req, HttpServletResponse res, WebContext ctx)
    throws IOException {
        String[] pathParts = req.getPathInfo().split("/");
        String operation   = pathParts[2];

        if (OP_EXPORT.equalsIgnoreCase(operation)) {
            exportFile(ff4j, res);
            return;

        } else if (OP_FEATURES.equalsIgnoreCase(operation)) {
            featuresAsJson(req, res);
            return;
            
        } else if (OP_PROPERTIES.equalsIgnoreCase(operation)) {
            propertiesAsJson(req, res);
            return;
            
        } else if (OP_FEATUREUSAGE.equalsIgnoreCase(operation)) {
            graphFeatureUsageHitCountasJson(req, res);
            return;
            
        } else if (OP_TIMESERIES.equalsIgnoreCase(operation)) {
            graphTimeSeriesasJson(req, res);
            return;
            
        } else if (OP_AUDIT.equalsIgnoreCase(operation)) {
            auditEventasJson(req, res); 
            return;
        } else {
            res.setStatus(WebConstants.STATUS_BADREQUEST);
            res.getWriter().println("Invalid request please check URL");
            return;
        }
    }
    
    /**
     * Generation of JSON to render Features.
     *
     * @param req
     *      current request
     * @param res
     *      current response
     * @throws IOException 
     */
    private void auditEventasJson(HttpServletRequest req, HttpServletResponse res)
    throws IOException {
        res.setContentType(CONTENT_TYPE_JSON);
        String[] pathParts = req.getPathInfo().split("/");
        if (pathParts.length > 3) {
            String eventUUId = pathParts[3];
            Long eventTime = null;
            if (isValidParam(req, WebConstants.KEY_DATE)) {
                eventTime = Long.valueOf(req.getParameter(WebConstants.KEY_DATE));
            }
            Event event = getFf4j().getEventRepository().getEventByUUID(eventUUId, eventTime);
            if (event != null) {
                res.getWriter().println(event.toString());
            } else {
                res.setStatus(WebConstants.STATUS_NOT_FOUND);
                res.getWriter().println("Event " + eventUUId + " does not exist in event repository." );
            }
        }
    }
    
    /**
     * Generation of JSON to render Features.
     *
     * @param req
     *      current request
     * @param res
     *      current response
     * @throws IOException 
     */
    private void graphTimeSeriesasJson(HttpServletRequest req, HttpServletResponse res)
    throws IOException {
        res.setContentType(CONTENT_TYPE_JSON);
        EventQueryDefinition query = parseQuery(req);
        TimeSeriesChart tsc = getFf4j().getEventRepository().getFeatureUsageHistory(query, TimeUnit.HOURS);
        res.getWriter().println(tsc.toString());
    }
   
    /**
     * Generation of JSON to render Features.
     *
     * @param req
     *      current request
     * @param res
     *      current response
     * @throws IOException 
     */
    private void graphFeatureUsageHitCountasJson(HttpServletRequest req, HttpServletResponse res)
    throws IOException {
        res.setContentType(CONTENT_TYPE_JSON);
        String[] pathParts = req.getPathInfo().split("/");
        EventQueryDefinition query = parseQuery(req);
        
        if (pathParts.length > 3) {
            String graphName = pathParts[3];
            if (GRAPH_PIE_HITRATIO.equalsIgnoreCase(graphName)) {
                PieChart pc = getFf4j().getEventRepository().getFeatureUsagePieChart(query);
                res.getWriter().println(pc.toJson());
            } else if (GRAPH_BAR_HITRATIO.equalsIgnoreCase(graphName)) {
                BarChart bc = getFf4j().getEventRepository().getFeatureUsageBarChart(query);
                res.getWriter().println(bc.toJson());
            } else if (GRAPH_PIE_HOST.equalsIgnoreCase(graphName)) {
                PieChart pc = getFf4j().getEventRepository().getHostPieChart(query);
                res.getWriter().println(pc.toJson());
                
            } else if (GRAPH_PIE_SOURCE.equalsIgnoreCase(graphName)) {
                PieChart pc = getFf4j().getEventRepository().getSourcePieChart(query);
                res.getWriter().println(pc.toJson());
                
            } else if (GRAPH_PIE_USER.equalsIgnoreCase(graphName)) {
                PieChart pc = getFf4j().getEventRepository().getUserPieChart(query);
                res.getWriter().println(pc.toJson());
            }
        }
    }
    
    /**
     * Generation of JSON to render Features.
     *
     * @param req
     *      current request
     * @param res
     *      current response
     * @throws IOException 
     */
    private void featuresAsJson(HttpServletRequest req, HttpServletResponse res)
    throws IOException {
        String[] pathParts = req.getPathInfo().split("/");
        res.setContentType(CONTENT_TYPE_JSON);
        if (pathParts.length > 3) {
            String featureId   = pathParts[3];
            if (getFf4j().getFeatureStore().exist(featureId)) {
                Feature f = getFf4j().getFeatureStore().read(featureId);
                res.getWriter().println(f.toJson());
            } else {

                res.setStatus(WebConstants.STATUS_NOT_FOUND);
                res.getWriter().println("Feature " + featureId + " does not exist in feature store." );
            }
        } else {
            Map< String, Feature > mapOfFeatures = getFf4j().getFeatureStore().readAll();
            StringBuilder sb = new StringBuilder("[");
            boolean first = true;
            for (Feature feature : mapOfFeatures.values()) {
                if (!first) {
                    sb.append(",");
                }
                sb.append(feature.toJson());
               first = false;
            }
            sb.append("]");
            res.getWriter().println(sb.toString());
        }
    }
    
    /**
     * Generation of JSON to render Properties.
     *
     * @param req
     *      current request
     * @param res
     *      current response
     */
    private void propertiesAsJson(HttpServletRequest req, HttpServletResponse res)
    throws IOException {
        String[] pathParts = req.getPathInfo().split("/");
        res.setContentType(CONTENT_TYPE_JSON);
        if (pathParts.length > 3) {
            String propertyName   = pathParts[3];
            if (getFf4j().getPropertiesStore().existProperty(propertyName)) {
                Property<?> p = getFf4j().getPropertiesStore().readProperty(propertyName);
                res.getWriter().println(p.toJson());
            } else {
                res.setStatus(WebConstants.STATUS_NOT_FOUND);
                res.getWriter().println("Property " + propertyName + " does not exist in property store." );
            }
        } else {
            Map< String, Property<?> > mapOfFeatures = getFf4j().getPropertiesStore().readAllProperties();
            StringBuilder sb = new StringBuilder("[");
            boolean first = true;
            for (Property<?> myProperty : mapOfFeatures.values()) {
                if (!first) {
                    sb.append(",");
                }
                sb.append(myProperty.toJson());
               first = false;
            }
            sb.append("]");
            res.getWriter().println(sb.toString());
        }
        return;
    }

}
