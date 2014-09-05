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

import javax.annotation.security.RolesAllowed;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;

import org.ff4j.FF4j;
import org.ff4j.core.FlippingExecutionContext;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.utils.json.FeatureJsonParser;
import org.ff4j.web.api.FF4jWebConstants;

/**
 * This is the parent class for FF4J the REST API.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Path("/ff4j")
@Produces(MediaType.APPLICATION_JSON)
@RolesAllowed({FF4jWebConstants.ROLE_READ})
public class FF4jResource implements FF4jWebConstants {

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
    @Path(RESOURCE_STORE)
    public FeatureStoreResource getFeaturesResource() {
        return new FeatureStoreResource(uriInfo, request, ff4j.getStore());
    }

    /**
     * Access monitoring part of the API.
     * 
     * @return monitoring resource
     */
    @Path(RESOURCE_MONITORING)
    public MonitoringResource getMonitoringResource() {
        return new MonitoringResource(uriInfo, request, ff4j.getEventRepository());
    }

    /**
     * Access security part of the API.
     *
     * @returning Security resource
     */
    @Path(RESOURCE_SECURITY)
    public AuthorizationManagerResource getAuthorizationManager() {
        return new AuthorizationManagerResource(uriInfo, request, ff4j.getAuthorizationsManager());
    }
    
    // Cache
    @Path(RESOURCE_CACHE)
    public CacheResource getCache() {
        System.out.println("getCache");
        return new CacheResource(uriInfo, request, ff4j.getStore());
    }

    /**
     * Provide core information on ff4J and available sub resources.
     */
    @GET
    public Response getStatus() {
        return Response.ok(ff4j.toString()).build();
    }

    /**
     * 
     * @param formParams
     * @return
     */
    @POST
    @Path(OPERATION_FLIP)
    @Consumes(MediaType.APPLICATION_FORM_URLENCODED)
    @Produces(MediaType.TEXT_PLAIN)
    public Response check(MultivaluedMap<String, String> formParams) {
        // Expected FeatureUID
        if (!formParams.containsKey(POST_PARAMNAME_FEATURE_UID)) {
            return Response.status(Response.Status.BAD_REQUEST)
                    .entity(POST_PARAMNAME_FEATURE_UID + " is a required POST parameter")
                    .build();
        }
        // Expected Custom FlipStrategy (JSON)
        String uid = formParams.getFirst(POST_PARAMNAME_FEATURE_UID);
        if (!ff4j.getStore().exist(uid)) {
            return Response.status(Response.Status.NOT_FOUND).entity(new FeatureNotFoundException(uid).getMessage()).build();
        }

        // If specific strategy is defined
        boolean flipped = false;
        if (formParams.containsKey(POST_PARAMNAME_FLIPSTRATEGY)) {
            FlippingStrategy fs = FeatureJsonParser.parseFlipStrategyAsJson(uid, formParams.getFirst(POST_PARAMNAME_FLIPSTRATEGY));
            FlippingExecutionContext fec = new FlippingExecutionContext();
            for (String key : formParams.keySet()) {
                if (!POST_PARAMNAME_FLIPSTRATEGY.equals(key) && !POST_PARAMNAME_FEATURE_UID.equals(key)) {
                    fec.putString(key, formParams.getFirst(key));
                }
            }
            flipped = ff4j.checkOveridingStrategy(uid, fs, fec);
        } else {
            flipped = ff4j.check(uid);
        }
        return Response.ok(String.valueOf(flipped)).build();
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
