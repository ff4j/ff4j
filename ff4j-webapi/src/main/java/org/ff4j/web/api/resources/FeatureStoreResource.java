package org.ff4j.web.api.resources;

import static org.ff4j.web.FF4jWebConstants.RESOURCE_CACHE;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_FEATURES;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_GROUPS;
import static org.ff4j.web.FF4jWebConstants.STORE_CLEAR;
import static org.ff4j.web.FF4jWebConstants.STORE_CREATESCHEMA;

/*
 * #%L
 * ff4j-web
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.annotation.security.RolesAllowed;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.ff4j.cache.FF4jCacheProxy;
import org.ff4j.core.Feature;
import org.ff4j.web.FF4jWebConstants;
import org.ff4j.web.api.resources.domain.CacheApiBean;
import org.ff4j.web.api.resources.domain.FeatureApiBean;
import org.ff4j.web.api.resources.domain.FeatureStoreApiBean;
import org.ff4j.web.api.resources.domain.GroupDescApiBean;
import org.ff4j.web.api.resources.domain.PropertyStoreApiBean;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * WebResource representing the store.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Path("/ff4j/store")
@Produces(MediaType.APPLICATION_JSON)
@RolesAllowed({FF4jWebConstants.ROLE_READ})
@Api(value = "/ff4j/store")
public class FeatureStoreResource extends AbstractResource {
    
    /**
     * Allows to retrieve feature by its id.
     * 
     * @param featId
     *            target feature identifier
     * @return feature is exist
     */
    @GET
    @ApiOperation(
            value= "Display information regarding to <b>Features</b>",
            notes= "other sub resources to be displayed",
            response=FeatureStoreApiBean.class)
    @ApiResponses(@ApiResponse(code = 200, message= "status of current ff4j bean"))
    @Produces(MediaType.APPLICATION_JSON)
    public FeatureStoreApiBean get() {
        return new FeatureStoreApiBean(ff4j.getFeatureStore());
    }
    
    @GET
    @Path("/" + RESOURCE_FEATURES)
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value= "Display information regarding <b>Features</b>", response=FeatureApiBean.class)
    @ApiResponses(@ApiResponse(code = 200, message= "get all features"))
    public List < FeatureApiBean> readFeatures() {
        Feature[] storeContent = getFeatureStore().readAll().values().toArray(new Feature[0]);
        List < FeatureApiBean > apiBean = new ArrayList<FeatureApiBean>();
        for (Feature feature : storeContent) {
            apiBean.add(new FeatureApiBean(feature));
        }
        return apiBean;
    }

    /**
     * Access groups part of the API.
     * 
     * @return groups resource
     */
    @GET
    @Path("/" + RESOURCE_GROUPS)
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value= "Display information regarding <b>Groups</b>", response=GroupDescApiBean.class)
    @ApiResponses({@ApiResponse(code = 200, message="Groups resource", response=GroupDescApiBean.class)})
    public List < GroupDescApiBean > readGroups() {
        Map< String, Feature > features = getFeatureStore().readAll();
        Map< String , GroupDescApiBean > groups = new HashMap<String, GroupDescApiBean>();
        if (features != null && !features.isEmpty()) {
            // Build groups from features
            for (Map.Entry<String,Feature> featureName : features.entrySet()) {
                String groupName = featureName.getValue().getGroup();
                // Add current group to list
                if (groupName != null && !groupName.isEmpty()) {
                    if (!groups.containsKey(groupName)) {
                        groups.put(groupName, new GroupDescApiBean(groupName, new ArrayList<String>()));
                    }
                    groups.get(groupName).getFeatures().add(featureName.getKey());
                }
            }
        }
        return new ArrayList<GroupDescApiBean>(groups.values());
    }
    
    @POST
    @Path("/" + STORE_CLEAR)
    @ApiOperation(value= "Delete all <b>Features</b> in store")
    @ApiResponses(@ApiResponse(code = 200, message= "status of current ff4j bean", response=FeatureStoreApiBean.class))
    @Produces(MediaType.APPLICATION_JSON)
    public FeatureStoreApiBean clearFeatures() {
        getFeatureStore().clear();
        return new FeatureStoreApiBean(ff4j.getFeatureStore());
    }
    
    @POST
    @Path("/" + STORE_CREATESCHEMA)
    @ApiOperation(value= "Create underlying DB schema for store")
    @ApiResponses(@ApiResponse(code = 200, message= "status of current ff4j bean", response=PropertyStoreApiBean.class))
    @Produces(MediaType.APPLICATION_JSON)
    public FeatureStoreApiBean createSchema() {
        getFeatureStore().createSchema();
        return new FeatureStoreApiBean(ff4j.getFeatureStore());
    }
    
    /**
     * Allows to retrieve feature by its id.
     * 
     * @param featId
     *            target feature identifier
     * @return feature is exist
     */
    @GET
    @Path("/" + RESOURCE_CACHE)
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value= "Display information related to <b>Cache</b>")
    @ApiResponses({ @ApiResponse(code = 200, message= "status of current ff4j monitoring bean", response=CacheApiBean.class),
                    @ApiResponse(code = 404, message= "no cache content provided") })
    public Response getStatus() {
        FF4jCacheProxy cacheProxy = ff4j.getCacheProxy();
        if (cacheProxy == null) {
            return Response.status(Response.Status.NOT_FOUND).entity("Current Store is not cached").build();
        }
        return Response.ok(new CacheApiBean(getFeatureStore())).build();
    }
    
    /**
     * POST Operation to clean cache.
     */
    @POST
    @Path("/" + RESOURCE_CACHE)
    @Produces(MediaType.TEXT_PLAIN)
    @ApiOperation(value= "Clear Cache", response=Response.class)
    @ApiResponses({ @ApiResponse(code = 200, message= "cache is cleard"),
                    @ApiResponse(code = 404, message= "no cache content provided") })
    public Response clear() {
        FF4jCacheProxy cacheProxy = ff4j.getCacheProxy();
        if (cacheProxy == null) {
            return Response.status(Response.Status.NOT_FOUND).entity("Current Store is not cached").build();
        }
        cacheProxy.getCacheManager().clearFeatures();
        return Response.ok("Cache has been cleared").build();
    }

}
