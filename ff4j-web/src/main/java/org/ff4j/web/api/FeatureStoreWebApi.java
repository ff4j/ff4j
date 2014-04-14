package org.ff4j.web.api;

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
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;

/**
 * This store will invoke a {@link RemoteHttpFeatureStore} to perform operations upon features. Call are done though http so
 * please consider to use some cache to limit
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Path("/ff4j/v1/store")
public class FeatureStoreWebApi {

    /** Access to Features through store. */
    private FeatureStore store = null;

    /** {@inheritDoc} */
    @PUT
    @Path("/features/{id}")
    @Consumes(MediaType.APPLICATION_JSON)
    public Response upsert(final Feature feature) {
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
     * Delete target feature if exist.
     * 
     * @param featId
     */
    @DELETE
    @Path("/features/{id}")
    public Response delete(@PathParam("id") String featId) {
        if (!getStore().exist(featId)) {
            // No header location as PUT and not POST
            return Response.status(Response.Status.NOT_FOUND).build();
        } else {
            getStore().delete(featId);
            return Response.status(Response.Status.NO_CONTENT).build();
        }
    }

    /**
     * Allows to retrieve feature by its id.
     * 
     * @param featId
     *            target feature identifier
     * @return feature is exist
     */
    @GET
    @Path("/features/{id}")
    @Produces(MediaType.APPLICATION_JSON)
    public Response read(@PathParam("id") String featId) {
        if (!getStore().exist(featId)) {
            // No header location as PUT and not POST
            return Response.status(Response.Status.NOT_FOUND).build();
        } else {
            Feature f = getStore().read(featId);
            return Response.ok(f).build();
        }
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
