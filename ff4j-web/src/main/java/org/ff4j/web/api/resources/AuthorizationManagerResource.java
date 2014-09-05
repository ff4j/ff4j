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
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;

import org.ff4j.security.AuthorizationsManager;
import org.ff4j.web.api.FF4jWebConstants;

/**
 * Resource to work with authorization manager.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@RolesAllowed({FF4jWebConstants.ROLE_READ})
public class AuthorizationManagerResource implements FF4jWebConstants {

    @Context
    private UriInfo uriInfo;

    @Context
    private Request request;
    
    /** Access to Features through store. */
    private AuthorizationsManager mnger = null;
    
    /**
     * Defaut constructor.
     */
    public AuthorizationManagerResource() {}
    
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
    public AuthorizationManagerResource(UriInfo uriInfo, Request request, AuthorizationsManager manager) {
        this.uriInfo = uriInfo;
        this.request = request;
        this.mnger = manager;
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
    public Response status() {
        if (mnger == null) {
            return Response.status(Response.Status.NOT_FOUND).build();
        }
        return Response.ok(mnger.toJson()).build();
    }    
    

}
