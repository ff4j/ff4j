package org.ff4j.web.api.resources;

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

import java.net.URI;
import java.net.URISyntaxException;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.annotation.security.RolesAllowed;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.ff4j.core.Feature;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.utils.MappingUtil;
import org.ff4j.web.FF4jWebConstants;
import org.ff4j.web.api.resources.domain.FeatureApiBean;
import org.ff4j.web.api.resources.domain.FlippingStrategyApiBean;
import org.ff4j.web.api.resources.domain.PropertyApiBean;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

import static org.ff4j.web.FF4jWebConstants.*;

/**
 * Represent a feature as WebResource.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Path("/ff4j/store/features/{uid}")
@Produces(MediaType.APPLICATION_JSON)
@RolesAllowed({FF4jWebConstants.ROLE_WRITE})
@Api(value = "/ff4j/store/features/{uid}")
public class FeatureResource extends AbstractResource {
  
    /**
     * Allows to retrieve feature by its id.
     * 
     * @param featId
     *            target feature identifier
     * @return feature is exist
     */
    @GET
    @RolesAllowed({ROLE_READ})
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value= "Read information about a feature", response=FeatureApiBean.class)
    @ApiResponses({
        @ApiResponse(code = 200, message= "Information about features"), 
        @ApiResponse(code = 404, message= "Feature not found") })
    public Response read(@PathParam("uid") String id) {
       if (!ff4j.getFeatureStore().exist(id)) {
            String errMsg = new FeatureNotFoundException(id).getMessage();
            return Response.status(Response.Status.NOT_FOUND).entity(errMsg).build();
       }
       return  Response.ok(new FeatureApiBean(ff4j.getFeatureStore().read(id))).build();
    }

    /**
     * Create the feature if not exist or update it
     * 
     * @param headers
     *            current request header
     * @param data
     *            feature serialized as JSON
     * @return 204 or 201
     */
    @PUT
    @RolesAllowed({ROLE_WRITE})
    @ApiOperation(value= "Create of update a feature", response=Response.class)
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    @ApiResponses({
        @ApiResponse(code = 201, message= "Feature has been created"), 
        @ApiResponse(code = 204, message= "No content, feature is updated") })
    public Response upsertFeature(@Context HttpHeaders headers, @PathParam("uid") String id, FeatureApiBean fApiBean) {
        // Parameter validations
        if ("".equals(id) || !id.equals(fApiBean.getUid())) {
            String errMsg = "Invalid identifier expected " + id;
            return Response.status(Response.Status.BAD_REQUEST).entity(errMsg).build();
        }
        
        Feature feat = new Feature(id);
        feat.setDescription(fApiBean.getDescription());
        feat.setEnable(fApiBean.isEnable());
        feat.setGroup(fApiBean.getGroup());
        feat.setPermissions(new HashSet<String>(fApiBean.getPermissions()));
        // Flipping Strategy
        FlippingStrategyApiBean flipApiBean = fApiBean.getFlippingStrategy();
        if (flipApiBean != null) {
            try {
                Map<String, String> initparams = flipApiBean.getInitParams();
                feat.setFlippingStrategy(MappingUtil.instanceFlippingStrategy(id, flipApiBean.getType(), initparams));
            } catch (Exception e) {
                String errMsg = "Cannot read Flipping Strategy, does not seems to have a DEFAULT constructor, " + e.getMessage();
                return Response.status(Response.Status.BAD_REQUEST).entity(errMsg).build();
            }
        }
        // Properties
        Map<String, PropertyApiBean> mapProperties = fApiBean.getCustomProperties();
        if (mapProperties != null) {
            for(PropertyApiBean propertyBean : mapProperties.values()) {
                feat.addProperty(propertyBean.asProperty());
            }       
        }
        
        // Update or create ? 
        if (!getFeatureStore().exist(feat.getUid())) {
            getFeatureStore().create(feat);
            String location = String.format("%s", uriInfo.getAbsolutePath().toString());
            try {
                return Response.created(new URI(location)).build();
            } catch (URISyntaxException e) {
                return Response.status(Response.Status.CREATED).header(LOCATION, location).entity(id).build();
            }
        }
        
        // Create
        getFeatureStore().update(feat);
        return Response.noContent().build();
    }

    /**
     * Delete feature by its id.
     * 
     * @return delete by its id.
     */
    @DELETE
    @RolesAllowed({ROLE_WRITE})
    @Produces(MediaType.TEXT_PLAIN)
    @ApiOperation(value= "Delete a feature", response=Response.class)
    @ApiResponses({
        @ApiResponse(code = 404, message= "Feature has not been found"), 
        @ApiResponse(code = 204, message= "No content, feature is deleted"),
        @ApiResponse(code = 400, message= "Bad identifier"),
        })
    public Response deleteFeature(@PathParam("uid") String id) {
        if (id == null || "".equals(id)) {
            String errMsg = "Invalid URL : Must be '/features/{uid}' with {uid} not null nor empty";
            return Response.status(Response.Status.BAD_REQUEST).entity(errMsg).build();
        }
        if (!ff4j.getFeatureStore().exist(id)) {
            String errMsg = new FeatureNotFoundException(id).getMessage();
            return Response.status(Response.Status.NOT_FOUND).entity(errMsg).build();
        }
        getFeatureStore().delete(id);
        return Response.noContent().build();
    }

    /**
     * Convenient method to update partially the feature: Here enabling
     * 
     * @return http response.
     */
    @POST
    @Path("/" + OPERATION_ENABLE)
    @RolesAllowed({ROLE_WRITE})
    @ApiOperation(value= "Enable a feature", response=Response.class)
    @ApiResponses({
        @ApiResponse(code = 204, message= "Features has been enabled"), 
        @ApiResponse(code = 404, message= "Feature not found") })
    public Response operationEnable(@PathParam("uid") String id) {
        if (!ff4j.getFeatureStore().exist(id)) {
            String errMsg = new FeatureNotFoundException(id).getMessage();
            return Response.status(Response.Status.NOT_FOUND).entity(errMsg).build();
        }
        getFeatureStore().enable(id);
        return Response.noContent().build();
    }

    /**
     * Convenient method to update partially the feature: Here disabling
     * 
     * @return http response.
     */
    @POST
    @Path("/" + OPERATION_DISABLE)
    @RolesAllowed({ROLE_WRITE})
    @ApiOperation(value= "Disable a feature", response=Response.class)
    @ApiResponses({
        @ApiResponse(code = 204, message= "Features has been disabled"), 
        @ApiResponse(code = 404, message= "Feature not found") })
    public Response operationDisable(@PathParam("uid") String id) {
        if (!ff4j.getFeatureStore().exist(id)) {
            String errMsg = new FeatureNotFoundException(id).getMessage();
            return Response.status(Response.Status.NOT_FOUND).entity(errMsg).build();
        }
        getFeatureStore().disable(id);
        return Response.noContent().build();
    }

    /**
     * Convenient method to update partially the feature: Here grant a role
     * 
     * @return http response.
     */
    @POST
    @RolesAllowed({ROLE_WRITE})
    @Path("/" + OPERATION_GRANTROLE + "/{role}" )
    @ApiOperation(value= "Grant a permission on a feature", response=Response.class)
    @ApiResponses({
        @ApiResponse(code = 204, message= "Permission has been granted"), 
        @ApiResponse(code = 404, message= "Feature not found"),
        @ApiResponse(code = 400, message= "Invalid RoleName") })
    public Response operationGrantRole(@PathParam("uid") String id, @PathParam("role") String role) {
        if (!ff4j.getFeatureStore().exist(id)) {
            String errMsg = new FeatureNotFoundException(id).getMessage();
            return Response.status(Response.Status.NOT_FOUND).entity(errMsg).build();
        }
        if ("".equals(role)) {
            String errMsg = "Invalid role should not be null nor empty";
            return Response.status(Response.Status.BAD_REQUEST).entity(errMsg).build();
        }
        getFeatureStore().grantRoleOnFeature(id, role);
        return Response.noContent().build();
    }

    /**
     * Convenient method to update partially the feature: Here removing a role
     * 
     * @return http response.
     */
    @POST
    @RolesAllowed({ROLE_WRITE})
    @Path("/" + OPERATION_REMOVEROLE + "/{role}" )
    @ApiOperation(value= "Remove a permission on a feature", response=Response.class)
    @ApiResponses({
        @ApiResponse(code = 204, message= "Permission has been granted"), 
        @ApiResponse(code = 404, message= "Feature not found"),
        @ApiResponse(code = 400, message= "Invalid RoleName") })
    public Response operationRemoveRole(@PathParam("uid") String id, @PathParam("role") String role) {
        if (!ff4j.getFeatureStore().exist(id)) {
            String errMsg = new FeatureNotFoundException(id).getMessage();
            return Response.status(Response.Status.NOT_FOUND).entity(errMsg).build();
        }
        Set < String > permissions = ff4j.getFeatureStore().read(id).getPermissions();
        if (!permissions.contains(role)) {
            String errMsg = "Invalid role should be within " + permissions;
            return Response.status(Response.Status.BAD_REQUEST).entity(errMsg).build();
        }
        getFeatureStore().removeRoleFromFeature(id, role);
        return Response.noContent().build();
    }
    
    /**
     * Convenient method to update partially the feature: Adding to a group
     * 
     * @return http response.
     */
    @POST
    @RolesAllowed({ROLE_WRITE})
    @Path("/" + OPERATION_ADDGROUP  + "/{groupName}" )
    @ApiOperation(value= "Define the group of the feature", response=Response.class)
    @ApiResponses({
        @ApiResponse(code = 204, message= "Group has been defined"), 
        @ApiResponse(code = 404, message= "Feature not found"),
        @ApiResponse(code = 400, message= "Invalid GroupName") })
    public Response operationAddGroup(@PathParam("uid") String id, @PathParam("groupName") String groupName) {
        if (!ff4j.getFeatureStore().exist(id)) {
            String errMsg = new FeatureNotFoundException(id).getMessage();
            return Response.status(Response.Status.NOT_FOUND).entity(errMsg).build();
        }
        if ("".equals(groupName)) {
            String errMsg = "Invalid groupName should not be null nor empty";
            return Response.status(Response.Status.BAD_REQUEST).entity(errMsg).build();
        }
        getFeatureStore().addToGroup(id, groupName);
        return Response.noContent().build();
    }
    
    /**
     * Convenient method to update partially the feature: Removing from a group
     * 
     * @return http response.
     */
    @POST
    @RolesAllowed({ROLE_WRITE})
    @Path("/" + OPERATION_REMOVEGROUP  + "/{groupName}")
    @ApiOperation(value= "Remove the group of the feature", response=Response.class)
    @ApiResponses({
        @ApiResponse(code = 204, message= "Group has been removed"), 
        @ApiResponse(code = 404, message= "Feature not found"),
        @ApiResponse(code = 400, message= "Invalid GroupName") })
    public Response operationRemoveGroup(@PathParam("uid") String id,  @PathParam("groupName") String groupName) {
        if (!ff4j.getFeatureStore().exist(id)) {
            String errMsg = new FeatureNotFoundException(id).getMessage();
            return Response.status(Response.Status.NOT_FOUND).entity(errMsg).build();
        }
        // Expected behaviour is no error even if invalid groupname
        // .. but invalid if group does not exist... 
        if (!ff4j.getFeatureStore().existGroup(groupName)) {
            String errMsg = "Invalid groupName should be " + groupName;
            return Response.status(Response.Status.BAD_REQUEST).entity(errMsg).build();
        }
        getFeatureStore().removeFromGroup(id, groupName);
        return Response.noContent().build();
    }

}
