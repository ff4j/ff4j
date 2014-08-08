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
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;

import org.ff4j.audit.EventRepository;
import org.ff4j.web.api.FF4jWebConstants;

/**
 * Web Resource to browse monitoring informations.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@RolesAllowed({FF4jWebConstants.ROLE_READ})
public class MonitoringResource implements FF4jWebConstants {
   
    @Context
    private UriInfo uriInfo;

    @Context
    private Request request;
    
    /** Repository of events. */
    private EventRepository evtRepository;

    /**
     * Defaut constructor.
     */
    public MonitoringResource() {}

    /**
     * Constructor by Parent resource
     * 
     * @param uriInfo
     *            current uriInfo
     * @param request
     *            current request
     */
    public MonitoringResource(UriInfo uriInfo, Request request, EventRepository evtRepo) {
        this.uriInfo = uriInfo;
        this.request = request;
        this.evtRepository = evtRepo;
    }

    /**
     * Provide core information on store and available sub resources.
     */
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public Response getStatus() {
        String jsonResponse = null;
        if (evtRepository != null) {
            jsonResponse = evtRepository.toString();
        }
        return Response.ok(jsonResponse).build();
    }
    
    /**
     * Access {@link FeatureResource} from path pattern
     * 
     * @param uid
     *            target identifier
     * @return resource for feature
     */
    @Path(RESOURCE_HITCURVES)
    public MonitorCurvesResource getCurveResources() {
        return new MonitorCurvesResource(uriInfo, request, evtRepository);
    }
    
    
}
