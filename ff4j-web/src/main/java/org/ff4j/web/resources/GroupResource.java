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
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.utils.FeatureJsonMarshaller;
import org.ff4j.web.api.FF4jWebConstants;

/**
 * WebResource representing a group of features.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class GroupResource implements FF4jWebConstants {

    @Context
    private UriInfo uriInfo;

    @Context
    private Request request;

    /** Access to Features through store. */
    private FeatureStore store = null;

    /** current groupName. */
    private String groupName = null;

    /**
     * Defaut constructor.
     */
    public GroupResource() {}

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
    public GroupResource(UriInfo uriInfo, Request request, FeatureStore store, String group) {
        this.uriInfo = uriInfo;
        this.request = request;
        this.store = store;
        this.groupName = group;
    }

    /**
     * Convenient method to work on groupd : Here enabling
     * 
     * @return http response.
     */
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public Response read() {
        Feature[] storeContent = getStore().readGroup(groupName).values().toArray(new Feature[0]);
        String storeAsJson = FeatureJsonMarshaller.marshallFeatureArray(storeContent);
        return Response.ok(storeAsJson).build();
    }

    /**
     * Convenient method to work on groupd : Here enabling
     * 
     * @return http response.
     */
    @POST
    @Path(OPERATION_ENABLE)
    public Response operationEnable() {
        getStore().enableGroup(groupName);
        return Response.noContent().build();
    }

    /**
     * Convenient method to work on groupd : Here enabling
     * 
     * @return http response.
     */
    @POST
    @Path(OPERATION_DISABLE)
    public Response operationDisableGroup() {
        getStore().disableGroup(groupName);
        return Response.noContent().build();
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
     * Getter accessor for attribute 'groupName'.
     *
     * @return current value of 'groupName'
     */
    public String getGroupName() {
        return groupName;
    }

}
