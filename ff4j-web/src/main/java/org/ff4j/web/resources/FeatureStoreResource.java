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

import org.ff4j.core.FeatureStore;
import org.ff4j.web.api.FF4jWebConstants;

/**
 * WebResource representing the store.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreResource implements FF4jWebConstants {

    @Context
    private UriInfo uriInfo;

    @Context
    private Request request;

    /** Access to Features through store. */
    private FeatureStore store = null;

    /**
     * Defaut constructor.
     */
    public FeatureStoreResource() {}

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
    public FeatureStoreResource(UriInfo uriInfo, Request request, FeatureStore store) {
        this.uriInfo = uriInfo;
        this.request = request;
        this.store = store;
    }

    /**
     * Access features part of the API.
     *
     * @return features resource
     */
    @Path(RESOURCE_FEATURES)
    public FeaturesResource getFeaturesResource() {
        return new FeaturesResource(uriInfo, request, store);
    }

    /**
     * Access groups part of the API.
     * 
     * @return groups resource
     */
    @Path(RESOURCE_GROUPS)
    public GroupsResource getGroupsResource() {
        return new GroupsResource(uriInfo, request, store);
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
    public Response describe() {
        return Response.ok(store.toString()).build();
    }

    /**
     * Getter accessor for attribute 'ff4j'.
     * 
     * @return current value of 'ff4j'
     */
    public FeatureStore getStore() {
        return store;
    }

    /**
     * Setter accessor for attribute 'store'.
     * 
     * @param store
     *            new value for 'store '
     */
    public void setStore(FeatureStore store) {
        this.store = store;
    }

}
