package org.ff4j.web.services;

/*
 * #%L FeatureWebService.java (ff4j-web) by Cedrick LUNVEN %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the
 * License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;

/**
 * This store will invoke a {@link RemoteHttpFeatureStore} to perform operations upon features. Call are done though http so
 * please consider to use some cache to limit
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Path("/ff4j/")
public class FeatureWebService {

    /** Objet serialise par JSON. */
    private static final String JSON_UTF8 = MediaType.APPLICATION_JSON + ";charset=UTF-8";

    /** Access to Features through store. */
    private FeatureStore store = null;

    /**
     * Exposition of {@link FeatureStore} as HTTP Service, method enable
     * 
     * @param featureId
     * @return
     */
    @GET
    @Path("/enable/{id}")
    @Produces(JSON_UTF8)
    public boolean enableWS(@PathParam("id") String featureId) {
        getStore().enable(featureId);
        return true;
    }

    /** {@inheritDoc} */
    @GET
    @Path("/disable/{id}")
    @Produces(JSON_UTF8)
    public boolean disable(@PathParam("id") String featureId) {
        getStore().disable(featureId);
        return true;
    }

    /** {@inheritDoc} */
    @GET
    @Path("/exist/{id}")
    @Produces(JSON_UTF8)
    public boolean exist(@PathParam("id") String featureId) {
        return getStore().exist(featureId);
    }

    /** {@inheritDoc} */
    @GET
    @Path("/read/{id}")
    @Produces(JSON_UTF8)
    public Feature read(@PathParam("id") String featureId) {
        return getStore().read(featureId);
    }

    /** {@inheritDoc} */
    @GET
    @Path("/list")
    @Produces(JSON_UTF8)
    public Feature[] readAll() {
        return getStore().readAll().values().toArray(new Feature[0]);
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
