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
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;

import org.ff4j.cache.FeatureCacheManager;
import org.ff4j.core.FeatureStore;
import org.ff4j.web.api.FF4jWebConstants;

/**
 * Web Resource to work on vache.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@RolesAllowed({FF4jWebConstants.ROLE_READ})
public class CacheResource implements FF4jWebConstants {

    @Context
    private UriInfo uriInfo;

    @Context
    private Request request;

    /** Access to Features through store. */
    private FeatureStore store = null;

    /**
     * Defaut constructor.
     */
    public CacheResource() {}

    /**
     * Constructor by Parent resource
     * 
     * @param uriInfo
     *            current uriInfo
     * @param request
     *            current request
     * @param store
     *            current store
     */
    public CacheResource(UriInfo uriInfo, Request request, FeatureStore store) {
        this.uriInfo = uriInfo;
        this.request = request;
        this.store = store;
    }

    /**
     * Allows to retrieve feature by its id.
     * 
     * @param featId
     *            target feature identifier
     * @return feature is exist
     */
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public Response getStatus() {
        if (!store.isCached()) {
            System.out.println("KO");
            return Response.status(Response.Status.NOT_FOUND).entity("No cache proxy set up").build();
        }
        StringBuilder sb = new StringBuilder("{");
        sb.append(",\"cacheProvider\":\"" + store.getCacheProvider() + "\"");
        sb.append(",\"cacheStore\":\"" + store.getCachedTargetStore() + "\"}");
        return Response.ok(sb.toString()).build();
    }
    
    /**
     * POST Operation to clean cache.
     */
    @POST
    @Produces(MediaType.TEXT_PLAIN)
    public Response cleanCache() {
        if (!store.isCached()) {
            return Response.status(Response.Status.NOT_FOUND).build();
        }
        ((FeatureCacheManager) store).clear();
        return Response.ok("Cache has been cleared").build();
    }

    /**
     * Getter accessor for attribute 'store'.
     *
     * @return current value of 'store'
     */
    public FeatureStore getStore() {
        return store;
    }

}
