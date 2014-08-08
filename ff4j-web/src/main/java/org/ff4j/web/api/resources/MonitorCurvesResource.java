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

import java.util.Set;

import javax.annotation.security.RolesAllowed;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.UriInfo;

import org.ff4j.audit.EventRepository;
import org.ff4j.web.api.FF4jWebConstants;

/**
 * Class to TODO
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@RolesAllowed({FF4jWebConstants.ROLE_READ})
public class MonitorCurvesResource implements FF4jWebConstants {

    @Context
    private UriInfo uriInfo;

    @Context
    private Request request;

    /** Repository of events. */
    private EventRepository evtRepository;

    /**
     * Defaut constructor.
     */
    public MonitorCurvesResource() {}

    /**
     * Constructor by Parent resource
     * 
     * @param uriInfo
     *            current uriInfo
     * @param request
     *            current request
     */
    public MonitorCurvesResource(UriInfo uriInfo, Request request, EventRepository evtRepo) {
        this.uriInfo = uriInfo;
        this.request = request;
        this.evtRepository = evtRepo;
    }

    /**
     * Access resources /ff4j/monitoring/curves and list curves
     *
     * @return return curves as a list of link
     */
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public Response getStatus() {
        StringBuilder res = new StringBuilder("{");
        Set<String> curves = evtRepository.getCurveList();
        if (null != curves) {
            boolean first = true;
            for (String curve : curves) {
                if (!first) {
                    res.append(",");
                }
                res.append("\"" + curve + "\":\"./curve/" + curve + "\"");
                first = false;
            }
        }
        res.append("}");
        return Response.ok(res.toString()).build();
    }

    /**
     * Access {@link FeatureResource} from path pattern
     * 
     * @param uid
     *            target identifier
     * @return resource for feature
     */
    @Path("{curve}")
    public MonitorCurveResource getCurve(@PathParam("curve") String featureName,
            @QueryParam("startTime") @DefaultValue("0") Long start, @QueryParam("endTime") Long end,
            @QueryParam("interval") Long interval) {
        // day = YYYY-MM-DD
        // 100 values calaulated
        if (end == null) {
            end = System.currentTimeMillis();
        }
        return null;
    }


}
