package org.ff4j.web.api.resources;

import static org.ff4j.web.FF4jWebConstants.PARAM_END;
import static org.ff4j.web.FF4jWebConstants.PARAM_START;

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
import java.util.Date;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.web.api.resources.domain.EventRepositoryApiBean;
import org.ff4j.web.api.resources.domain.FeatureMonitoringApiBean;
import org.ff4j.web.api.resources.domain.PieSectorApiBean;

import com.fasterxml.jackson.annotation.JsonIgnoreType;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;

/**
 * Monitoring Resource.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Path("/ff4j/monitoring")
@JsonIgnoreType
@Api(value = "/ff4j/monitoring")
public class MonitoringResource extends AbstractResource {
    
    /**
     * Provide core information on store and available sub resources.
     */
    @GET
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value= "Display <b>Monitoring</b> information for <b><u>all</u></b> features",
                  notes= "The <b>EventRepository</b> handle to store audit events is not required", 
                  response=EventRepositoryApiBean.class)
    @ApiResponses({ 
        @ApiResponse(code = 200, message= "Status of event repository bean", response=EventRepositoryApiBean.class),
        @ApiResponse(code = 404, message= "No event repository defined",     response=String.class)
    })
    public Response getMonitoringStatus(
            @ApiParam(required=false, name="start", value="Start of window <br>(default is today 00:00)")
            @QueryParam(PARAM_START) Long start,
            @ApiParam(required=false, name="end", value="End  of window <br>(default is tomorrow 00:00)")
            @QueryParam(PARAM_END) Long end) {
        if (null == getRepo()) {
            return Response.status(Status.NOT_FOUND).entity("No monitoring has been defined").build();
        }
        return Response.ok(new EventRepositoryApiBean(getRepo(), start, end)).build();
    }
    
    /**
     * Provide core information on store and available sub resources.
     */
    @GET
    @Path("/{uid}")
    @Produces(MediaType.APPLICATION_JSON)
    @ApiOperation(value= "Display <b>Monitoring</b> for a <b><u>single</u></b> feature",
                  notes= "Each feature will display a pieChart and a barChart for hits",
                  response=FeatureMonitoringApiBean.class)
    @ApiResponses({
        @ApiResponse(code = 200, message= "Status of current ff4j monitoring bean", response=FeatureMonitoringApiBean.class), 
        @ApiResponse(code = 404, message= "Feature not found", response=String.class) })
    public Response getFeatureMonitoring(
            @ApiParam(required=true, name="uid", value="Unique identifier of feature")
            @PathParam("uid") String uid, 
            @ApiParam(required=false, name="start", value="Start of window <br>(default is today 00:00)")
            @QueryParam(PARAM_START) Long start,
            @ApiParam(required=false, name="end", value="End  of window <br>(default is tomorrow 00:00)")
            @QueryParam(PARAM_END) Long end) {
        if (!ff4j.getFeatureStore().exist(uid)) {
            String errMsg = new FeatureNotFoundException(uid).getMessage();
            return Response.status(Response.Status.NOT_FOUND).entity(errMsg).build();
        }
        // Today
        Calendar c = Calendar.getInstance();
        c.set(Calendar.HOUR_OF_DAY, 0);
        c.set(Calendar.MINUTE, 0);
        c.set(Calendar.SECOND, 0);
        if (start == null) {
            start = c.getTimeInMillis();
        }
        // Tomorrow 00:00
        Calendar c2 = Calendar.getInstance();
        c2.setTime(new Date(System.currentTimeMillis() + 1000 * 3600 * 24));
        c2.set(Calendar.HOUR_OF_DAY, 0);
        c2.set(Calendar.MINUTE, 0);
        c2.set(Calendar.SECOND, 0);
        if (end == null) {
            end = c2.getTimeInMillis();
        }
        // Build response
        FeatureMonitoringApiBean fmab = new FeatureMonitoringApiBean(uid);
        int hitcount = 0;
        for (PieSectorApiBean sec : fmab.getEventsPie().getSectors()) {
            hitcount+= sec.getValue();
        }
        fmab.setHitCount(hitcount);
        return Response.ok().entity(fmab).build();
    }

    
}
