package org.ff4j.web.api.resources;

import java.util.ArrayList;
import java.util.List;

import javax.annotation.security.RolesAllowed;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.ff4j.cache.FF4jCacheProxy;
import org.ff4j.property.Property;
import org.ff4j.web.FF4jWebConstants;
import org.ff4j.web.api.resources.domain.CacheApiBean;
import org.ff4j.web.api.resources.domain.FeatureStoreApiBean;
import org.ff4j.web.api.resources.domain.PropertyApiBean;
import org.ff4j.web.api.resources.domain.PropertyStoreApiBean;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

import static org.ff4j.web.FF4jWebConstants.*;

/*
 * #%L
 * ff4j-webapi
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
@Path("/ff4j/propertyStore")
@Produces(MediaType.APPLICATION_JSON)
@RolesAllowed({FF4jWebConstants.ROLE_READ})
@Api(value = "/ff4j/propertyStore")
public class PropertyStoreResource  extends AbstractResource {
  
    /**
     * Allows to retrieve feature by its id.
     * 
     * @param featId
     *            target feature identifier
     * @return feature is exist
     */
    @GET
    @ApiOperation(
            value= "Display information regarding to <b>Properties</b>",
            notes= "other sub resources to be displayed",
            response=FeatureStoreApiBean.class)
    @ApiResponses(@ApiResponse(code = 200, message= "status of current ff4j bean"))
    @Produces(MediaType.APPLICATION_JSON)
    public PropertyStoreApiBean get() {
        return new PropertyStoreApiBean(ff4j.getPropertiesStore());
    }
    
    @GET
    @Path("/" + RESOURCE_PROPERTIES)
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value= "Display information regarding <b>Properties</b>", response=PropertyApiBean.class)
    @ApiResponses(@ApiResponse(code = 200, message= "get all Properties"))
    public List < PropertyApiBean> readProperties() {
        List < PropertyApiBean > apiBean = new ArrayList<PropertyApiBean>();
        getPropertyStore().readAllProperties();
        for (Property<?> prop : getPropertyStore().readAllProperties().values()) {
            apiBean.add(new PropertyApiBean(prop));
        }
        return apiBean;
    }
    
    @POST
    @Path("/" + STORE_CLEAR)
    @ApiOperation(value= "Delete all <b>Properties</b> in store")
    @ApiResponses(@ApiResponse(code = 200, message= "status of current ff4j bean", response=PropertyStoreApiBean.class))
    @Produces(MediaType.APPLICATION_JSON)
    public PropertyStoreApiBean clearProperties() {
        getPropertyStore().clear();
        return new PropertyStoreApiBean(ff4j.getPropertiesStore());
    }
    
    @POST
    @Path("/" + STORE_CREATESCHEMA)
    @ApiOperation(value= "Create underlying DB schema for store")
    @ApiResponses(@ApiResponse(code = 200, message= "status of current ff4j bean", response=PropertyStoreApiBean.class))
    @Produces(MediaType.APPLICATION_JSON)
    public PropertyStoreApiBean createSchema() {
        getPropertyStore().createSchema();
        return new PropertyStoreApiBean(ff4j.getPropertiesStore());
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
        cacheProxy.getCacheManager().clearProperties();
        return Response.ok("Cache has been cleared").build();
    }

}
