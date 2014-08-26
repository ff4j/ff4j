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
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.UriInfo;

import org.ff4j.audit.EventRepository;
import org.ff4j.web.api.FF4jWebConstants;

/**
 * Super class to inject event repository in all monitoring resources.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@RolesAllowed({FF4jWebConstants.ROLE_READ})
public abstract class MonitorAbstractResource implements FF4jWebConstants {

    @Context
    protected UriInfo uriInfo;

    @Context
    protected Request request;
    
    /** Repository of events. */
    private EventRepository evtRepository;
    
    /**
     * Defaut constructor.
     */
    public MonitorAbstractResource() {}

    /**
     * Constructor by Parent resource
     * 
     * @param uriInfo
     *            current uriInfo
     * @param request
     *            current request
     */
    public MonitorAbstractResource(UriInfo uriInfo, Request request, EventRepository evtRepo) {
        this.uriInfo = uriInfo;
        this.request = request;
        this.evtRepository = evtRepo;
    }

    /**
     * Getter accessor for attribute 'evtRepository'.
     *
     * @return
     *       current value of 'evtRepository'
     */
    public EventRepository getEvtRepository() {
        return evtRepository;
    }

    /**
     * Setter accessor for attribute 'evtRepository'.
     * @param evtRepository
     * 		new value for 'evtRepository '
     */
    public void setEvtRepository(EventRepository evtRepository) {
        this.evtRepository = evtRepository;
    }


}
