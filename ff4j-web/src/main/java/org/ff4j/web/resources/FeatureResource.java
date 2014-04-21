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

import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.PUT;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.utils.FeatureJsonMarshaller;

public class FeatureResource {

    @Context
    private UriInfo uriInfo;

    @Context
    private Request request;

    private String id;

    private FeatureStore store;

    /**
     * Defaut constructor.
     */
    public FeatureResource() {}

    /**
     * Constructor by Parent resource
     * 
     * @param uriInfo
     *            current uriInfo
     * @param request
     *            current request
     */
    public FeatureResource(UriInfo uriInfo, Request request, String id, FeatureStore fStore) {
        this.uriInfo = uriInfo;
        this.request = request;
        this.id = id;
        this.store = fStore;
    }

    /**
     * Allows to retrieve feature by its id.
     * 
     * @param featId
     *            target feature identifier
     * @return feature is exist
     */
    @GET
    public Response read() {
        if (id == null || "".equals(id)) {
            return Response.status(Response.Status.BAD_REQUEST)
                    .entity("Invalid URL : Must be '/features/{id}' with {id} not null nor empty").build();
        }
        if (!getStore().exist(id)) {
            return Response.status(Response.Status.NOT_FOUND).entity(new FeatureNotFoundException(id).getMessage()).build();
        } else {
            return Response.ok(FeatureJsonMarshaller.marshallFeature(getStore().read(id))).build();
        }
    }

    @PUT
    @Consumes(MediaType.APPLICATION_JSON)
    public Response upsertFeature(final Feature feature) {
        if (!getStore().exist(feature.getUid())) {
            getStore().create(feature);
            // No header location as PUT and not POST
            return Response.status(Response.Status.CREATED).build();
        } else {
            getStore().update(feature);
            return Response.status(Response.Status.NO_CONTENT).build();
        }
    }

    /**
     * Delete feature by its id.
     * 
     * @return delete by its id.
     */
    @DELETE
    public Response deleteFeature() {
        if (id == null || "".equals(id)) {
            return Response.status(Response.Status.BAD_REQUEST)
                    .entity("Invalid URL : Must be '/features/{id}' with {id} not null nor empty").build();
        }
        if (!getStore().exist(id)) {
            return Response.status(Response.Status.NOT_FOUND).entity(new FeatureNotFoundException(id).getMessage()).build();
        } else {
            getStore().delete(id);
            return Response.status(Response.Status.NO_CONTENT).build();
        }
    }

    /**
     * Getter accessor for attribute 'store'.
     * 
     * @return current value of 'store'
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
