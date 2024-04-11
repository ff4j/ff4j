package org.ff4j.web.api.resources;

/*-
 * #%L
 * ff4j-webapi
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

import static org.ff4j.web.FF4jWebConstants.OPERATION_CHECK;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_SECURITY;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.annotation.security.RolesAllowed;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.*;
import javax.ws.rs.core.Response.Status;

import org.ff4j.core.FlippingExecutionContext;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.security.AuthorizationsManager;
import org.ff4j.web.FF4jWebConstants;
import org.ff4j.web.api.resources.domain.AuthorizationsManagerApiBean;
import org.ff4j.web.api.resources.domain.FF4jStatusApiBean;
import org.ff4j.web.api.security.FF4JSecurityContextHolder;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * This is the parent class for FF4J the REST API.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Path("/ff4j")
@Produces(MediaType.APPLICATION_JSON)
@RolesAllowed({FF4jWebConstants.ROLE_READ})
@Api(value = "/ff4j")
public class FF4jResource extends AbstractResource {
    
    /**
     * Provide core information on ff4J and available sub resources.
     * @return
     *      status bean
     */
    @GET
    @ApiOperation(
            value= "Display <b>ff4j</b> status overview",
            notes= "Display informations related to <b>Monitoring</b>, <b>Security</b>, <b>Cache</b> and <b>Store</b>")
    @ApiResponses(
                @ApiResponse(code = 200, message= "Success, return status of ff4j instance", response=FF4jStatusApiBean.class))
    @Produces(MediaType.APPLICATION_JSON)
    public FF4jStatusApiBean getStatus() {
       return new FF4jStatusApiBean(ff4j);
    }
    
    /**
     * Display security resources.
     * 
     * @return
     *      api bean, representation of authorization manager
     */
    @GET
    @Path("/" + RESOURCE_SECURITY)
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value= "Display <b>Security</b> informations (permissions manager)", 
    notes="Security is implemented through dedicated <b>AuthorizationsManager</b> but it's not mandatory")
    @ApiResponses({ @ApiResponse(code = 200, message= "Status of current ff4j security bean", response=AuthorizationsManagerApiBean.class),
                    @ApiResponse(code = 404, message= "No security defined, no response") })
    public Response getSecurityStatus() {
        AuthorizationsManager authMgner = ff4j.getAuthorizationsManager();
        if (null == authMgner) {
            return Response.status(Status.NOT_FOUND).entity("No security has been defined").build();
        }
        return Response.ok(new AuthorizationsManagerApiBean(authMgner)).build();
    }
    
    /**
     * Check if feature if flipped
     * 
     * @param formParams
     *      target custom params
     * @return
     *      boolean if feature if flipped
     */
    @GET
    @Path("/" + OPERATION_CHECK + "/{uid}") 
    @Produces(MediaType.TEXT_PLAIN)
    @ApiOperation(value= "<b>Simple check</b> feature toggle", response=Boolean.class)
    @ApiResponses({
        @ApiResponse(code = 200, message= "if feature is flipped"),
        @ApiResponse(code = 400, message= "Invalid parameter"),
        @ApiResponse(code = 404, message= "feature has not been found")})
    public Response check(@Context HttpHeaders headers, @PathParam("uid") String uid) {
        // HoldSecurity Context
        FF4JSecurityContextHolder.save(securityContext);
        
        // Expected Custom FlipStrategy (JSON)
        if (!ff4j.getFeatureStore().exist(uid)) {
            String errMsg = new FeatureNotFoundException(uid).getMessage();
            return Response.status(Response.Status.NOT_FOUND).entity(errMsg).build();
        }
        return Response.ok(String.valueOf(ff4j.check(uid))).build();
    }
    
    /**
     * Check if feature if flipped
     * 
     * @param formParams
     *      target custom params
     * @return
     *      boolean if feature if flipped
     */
    @POST
    @Path("/" + OPERATION_CHECK + "/{uid}") 
    @Produces(MediaType.TEXT_PLAIN)
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @ApiOperation(value= "<b>Advanced check</b> feature toggle (parameterized)", response=Boolean.class)
    @ApiResponses({
        @ApiResponse(code = 200, message= "if feature is flipped"),
        @ApiResponse(code = 400, message= "Invalid parameter")})
    public Response checkPOST(@Context HttpHeaders headers, @PathParam("uid") String uid, MultivaluedMap<String, String> formParams) {
        // HoldSecurity Context
        FF4JSecurityContextHolder.save(securityContext);
        if (!ff4j.getFeatureStore().exist(uid)) {
            String errMsg = new FeatureNotFoundException(uid).getMessage();
            return Response.status(Response.Status.NOT_FOUND).entity(errMsg).build();
       }

       // Flipping Strategy may expected some dedicated parameters if not present, will return 400
       FlippingExecutionContext flipExecCtx = new FlippingExecutionContext();
       for (String key : formParams.keySet()) {
           flipExecCtx.putString(key, formParams.getFirst(key));
       }
       try {
           boolean flipped = ff4j.check(uid, flipExecCtx);
           return Response.ok(String.valueOf(flipped)).build();
       } catch(IllegalArgumentException iae) {
           String errMsg = "Invalid parameter " + iae.getMessage();
           return Response.status(Response.Status.BAD_REQUEST).entity(errMsg).build();
       }
       
    }


    /**
     * Check if some features are flipped
     *
     * @return
     *      Map<String,Boolean> with featureUID and flipped
     */
    @POST
    @Path("/" + OPERATION_CHECK)
    @Produces(MediaType.APPLICATION_JSON)
    @Consumes(MediaType.APPLICATION_JSON)
    @ApiOperation(value= "<b>Check</b> multiple feature toggles", response=Boolean.class)
    @ApiResponses({
            @ApiResponse(code = 200, message= "Map of feature / flipped"),
            @ApiResponse(code = 400, message= "Invalid parameter")})
    public Response checkMulti(@Context HttpHeaders headers, @Context UriInfo uriInfo, Set<String> featureUIDs) {
        FF4JSecurityContextHolder.save(securityContext);
        final Map<String, Boolean> featureFlippedMap = new HashMap<String, Boolean>();
        final MultivaluedMap<String, String> formParams = uriInfo.getQueryParameters();
        final FlippingExecutionContext flipExecCtx = new FlippingExecutionContext();
        for (String key : formParams.keySet()) {
            flipExecCtx.putString(key, formParams.getFirst(key));
        }
        if (featureUIDs != null) {
            for (final String featureUID : featureUIDs) {
                try {
                    featureFlippedMap.put(featureUID,  ff4j.check(featureUID, flipExecCtx));
                } catch (FeatureNotFoundException e) {
                    featureFlippedMap.put(featureUID, false);
                }
            }
        }
        return Response.ok(featureFlippedMap).build();
    }
    
}
