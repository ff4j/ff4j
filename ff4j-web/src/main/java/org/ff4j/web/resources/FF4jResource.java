package org.ff4j.web.resources;

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

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;

import org.ff4j.FF4j;

/**
 * This is the parent class for FF4J the REST API.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Path("/ff4j")
@Produces(MediaType.APPLICATION_JSON)
public class FF4jResource {

    /** Access to Features through store. */
    @Context
    private FF4j ff4j = null;

    /** rest url. */
    @Context
    private UriInfo uriInfo;

    /** current request. */
    @Context
    private Request request;

    /**
     * Access features part of the API.
     *
     * @return features resource
     */
    @Path("features")
    public FeaturesResource getFeaturesResource() {
        return new FeaturesResource(uriInfo, request, ff4j.getStore());
    }
    
    /**
     * Access groups part of the API.
     * 
     * @return groups resource
     */
    @Path("groups")
    public GroupsResource getGroupsResource() {
        return new GroupsResource(uriInfo, request, ff4j.getStore());
    }

    /**
     * Access monitoring part of the API.
     * 
     * @return monitoring resource
     */
    @Path("monitoring")
    public MonitoringResource getMonitoringResource() {
        return new MonitoringResource(uriInfo, request);
    }

    /**
     * Provide core information on ff4J and available sub resources.
     */
    @GET
    public Response getStatus() {
        String jsonResponse = "{\"ff4j-stats\" : true }";
        return Response.ok(jsonResponse).build();
    }

    /**
     * Getter accessor for attribute 'ff4j'.
     * 
     * @return current value of 'ff4j'
     */
    public FF4j getFf4j() {
        return ff4j;
    }

    /**
     * Setter accessor for attribute 'ff4j'.
     * 
     * @param ff4j
     *            new value for 'ff4j '
     */
    public void setFf4j(FF4j ff4j) {
        this.ff4j = ff4j;
    }

}
