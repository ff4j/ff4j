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

import static org.ff4j.utils.json.FeatureJsonParser.featureArrayToJson;

import javax.annotation.security.RolesAllowed;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.ff4j.core.Feature;
import org.ff4j.web.FF4jWebConstants;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

import static org.ff4j.web.FF4jWebConstants.*;

/**
 * WebResource representing a group of features.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Produces(MediaType.APPLICATION_JSON)
@Path("/ff4j/store/groups/{groupName}")
@RolesAllowed({FF4jWebConstants.ROLE_READ})
@Api(value = "/ff4j/store/groups/{groupName}")
public class GroupResource extends AbstractResource {
 
    /**
     * Convenient method to work on groupd : Here enabling
     * 
     * @return http response.
     */
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value= "Read information about a group", response=Response.class)
    @ApiResponses({
        @ApiResponse(code = 200, message= "Information about target group"), 
        @ApiResponse(code = 404, message= "Group not found") })
    public Response read(@PathParam("groupName") String groupName) {
        Feature[] storeContent = getFeatureStore().readGroup(groupName).values().toArray(new Feature[0]);
        return Response.ok(featureArrayToJson(storeContent)).build();
    }

    /**
     * Convenient method to work on groupd : Here enabling
     * 
     * @return http response.
     */
    @POST
    @Path("/" + OPERATION_ENABLE)
    @RolesAllowed({ROLE_WRITE})
    @ApiOperation(value= "Enable a group", response=Response.class)
    @ApiResponses(@ApiResponse(code = 204, message= "Group has been updated"))
    public Response operationEnable(@PathParam("groupName") String groupName) {
        getFeatureStore().enableGroup(groupName);
        return Response.noContent().build();
    }

    /**
     * Convenient method to work on groupd : Here enabling
     * 
     * @return http response.
     */
    @POST
    @Path("/" + OPERATION_DISABLE)
    @RolesAllowed({ROLE_WRITE})
    @ApiOperation(value= "Disable a group", response=Response.class)
    @ApiResponses(@ApiResponse(code = 204, message= "Group has been disabled"))
    public Response operationDisableGroup(@PathParam("groupName") String groupName) {
        getFeatureStore().disableGroup(groupName);
        return Response.noContent().build();
    }

}
