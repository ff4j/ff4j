package org.ff4j.web.api.resources;

import java.net.URI;
import java.net.URISyntaxException;

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

import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.web.FF4jWebConstants;
import org.ff4j.web.api.resources.domain.PropertyApiBean;

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

/**
 * WebResource representing the store.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Path("/ff4j/propertyStore/properties/{name}")
@Produces(MediaType.APPLICATION_JSON)
@RolesAllowed({FF4jWebConstants.ROLE_WRITE})
@Api(value = "/ff4j/propertyStore/properties/{name}")
public class PropertyResource extends AbstractResource {
 
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
    @ApiOperation(value= "Read information about a Property", response=PropertyApiBean.class)
    @ApiResponses({
        @ApiResponse(code = 200, message= "Information about features"), 
        @ApiResponse(code = 404, message= "Property not found") })
    public Response read(@PathParam("name") String name) {
       if (!ff4j.getPropertiesStore().existProperty(name)) {
            String errMsg = new PropertyNotFoundException(name).getMessage();
            return Response.status(Response.Status.NOT_FOUND).entity(errMsg).build();
       }
       return Response.ok(new PropertyApiBean(ff4j.getPropertiesStore().readProperty(name))).build();
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
    @ApiOperation(value= "Create of update a Property", response=Response.class)
    @Consumes(MediaType.APPLICATION_JSON)
    @Produces(MediaType.APPLICATION_JSON)
    @ApiResponses({
        @ApiResponse(code = 201, message= "Property has been created"), 
        @ApiResponse(code = 204, message= "No content, feature is updated") })
    public Response upsertProperty(@Context HttpHeaders headers, @PathParam("name") String name, PropertyApiBean pApiBean) {
        // Parameter validations
        if ("".equals(name) || !name.equals(pApiBean.getName())) {
            String errMsg = "Invalid identifier expected " + name;
            return Response.status(Response.Status.BAD_REQUEST).entity(errMsg).build();
        }
        
        Property<?> property = pApiBean.asProperty();
        // Update or create ? 
        if (!getPropertyStore().existProperty(property.getName())) {
            getPropertyStore().createProperty(property);
            String location = String.format("%s", uriInfo.getAbsolutePath().toString());
            try {
                return Response.created(new URI(location)).build();
            } catch (URISyntaxException e) {
                return Response.status(Response.Status.CREATED).header(LOCATION, location).entity(name).build();
            }
        }
        
        // Create
        getPropertyStore().updateProperty(property);
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
    @ApiOperation(value= "Delete a Property", response=Response.class)
    @ApiResponses({
        @ApiResponse(code = 404, message= "Property has not been found"), 
        @ApiResponse(code = 204, message= "No content, Property is deleted"),
        @ApiResponse(code = 400, message= "Bad identifier"),
        })
    public Response deleteProperty(@PathParam("name") String name) {
        if (name == null || "".equals(name)) {
            String errMsg = "Invalid URL : Must be '/properties/{name}' with {name} not null nor empty";
            return Response.status(Response.Status.BAD_REQUEST).entity(errMsg).build();
        }
        if (!ff4j.getPropertiesStore().existProperty(name)) {
            String errMsg = new FeatureNotFoundException(name).getMessage();
            return Response.status(Response.Status.NOT_FOUND).entity(errMsg).build();
        }
        getPropertyStore().deleteProperty(name);
        return Response.noContent().build();
    }

    /**
     * Convenient method to update partially the feature: Here enabling
     * 
     * @return http response.
     */
    @POST
    @Path("/" + OPERATION_UPDATE)
    @RolesAllowed({ROLE_WRITE})
    @ApiOperation(value= "Update a property", response=Response.class)
    @ApiResponses({
        @ApiResponse(code = 204, message= "Property has been updated"), 
        @ApiResponse(code = 404, message= "Property not found"),
        @ApiResponse(code = 400, message= "Invalid new value") })
    public Response operationUpdate(@PathParam("name") String name, @PathParam("groupName") String newValue) {
        if (!ff4j.getPropertiesStore().existProperty(name)) {
            String errMsg = new FeatureNotFoundException(name).getMessage();
            return Response.status(Response.Status.NOT_FOUND).entity(errMsg).build();
        }
        getPropertyStore().updateProperty(name, newValue);
        return Response.noContent().build();
    }

}
