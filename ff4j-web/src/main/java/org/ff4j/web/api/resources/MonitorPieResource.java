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

import java.util.Calendar;

import javax.annotation.security.RolesAllowed;
import javax.ws.rs.GET;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;

import org.ff4j.audit.EventRepository;
import org.ff4j.web.api.FF4jWebConstants;

/**
 * Build hitcounts pies.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@RolesAllowed({FF4jWebConstants.ROLE_READ})
public class MonitorPieResource extends MonitorAbstractResource {
    
    /** target feature id (if present). */
    private String featureId = null;
    
    /**
     * Defaut constructor.
     */
    public MonitorPieResource() {}

    /**
     * Constructor by Parent resource
     * 
     * @param uriInfo
     *            current uriInfo
     * @param request
     *            current request
     */
    public MonitorPieResource(UriInfo uriInfo, Request request, EventRepository evtRepo) {
        super(uriInfo, request, evtRepo);
    }
    
    /**
     * Constructor by Parent resource
     * 
     * @param uriInfo
     *            current uriInfo
     * @param request
     *            current request
     */
    public MonitorPieResource(UriInfo uriInfo, Request request, EventRepository evtRepo, String uid) {
        this(uriInfo, request, evtRepo);
        this.featureId = uid;
    }
    
    /**
     * Provide core information on store and available sub resources.
     */
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public Response getPie(@QueryParam(PARAM_START) Long start, @QueryParam(PARAM_END) Long end) {
        long myStart = 0;
        if (null == start) {
            Calendar c = Calendar.getInstance();
            c.set(Calendar.HOUR_OF_DAY, 0);
            c.set(Calendar.MINUTE, 0);
            c.set(Calendar.SECOND, 0);
            myStart = c.getTimeInMillis();
        } else {
            myStart = start;
        }
        long myEnd = System.currentTimeMillis();
        if (null != end) {
            myEnd = end;
        }
        
        String jsonResponse = "ko";
        if (null != featureId) {
            // Compute Pie for dedicated feature
            jsonResponse = getEvtRepository().getFeatureHitsPie(featureId, myStart, myEnd).toString();
        } else {
            // Create the total hit pie
            jsonResponse = getEvtRepository().getTotalHitsPie(myStart, myEnd).toString();
        }
        return Response.ok(jsonResponse).build();
    }


}
