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

import java.util.Set;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;

import org.ff4j.core.FeatureStore;
import org.ff4j.web.api.FF4jWebApiConstants;

/**
 * Web Resource to work on groups.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class GroupsResource implements FF4jWebApiConstants {

    @Context
    private UriInfo uriInfo;

    @Context
    private Request request;

    /** Access to Features through store. */
    private FeatureStore store = null;

    /**
     * Defaut constructor.
     */
    public GroupsResource() {}

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
    public GroupsResource(UriInfo uriInfo, Request request, FeatureStore store) {
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
    public Response readAll() {
        Set<String> setOfGroup = getStore().readAllGroups();
        StringBuilder sb = new StringBuilder(" {");
        boolean first = true;
        for (String group : setOfGroup) {
            if (!first) {
                sb.append(",");
            }
            first = false;
            sb.append("\"" + group + "\":\"" + uriInfo.getAbsolutePath() + group + "\"");
        }
        sb.append("}");
        return Response.ok(sb.toString()).build();
    }

    /**
     * Access {@link FeatureResource} from path pattern
     * 
     * @param uid
     *            target identifier
     * @return resource for feature
     */
    @Path("{groupName}")
    public GroupResource getGroup(@PathParam("groupName") String groupName) {
        return new GroupResource(uriInfo, request, store, groupName);
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
