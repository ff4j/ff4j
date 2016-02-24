package org.ff4j.web.api.filter;

/*
 * #%L
 * ff4j-webapi-jersey2x
 * %%
 * Copyright (C) 2013 - 2015 FF4J
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


import javax.ws.rs.ext.Provider;

import org.glassfish.jersey.server.monitoring.ApplicationEvent;
import org.glassfish.jersey.server.monitoring.ApplicationEventListener;
import org.glassfish.jersey.server.monitoring.RequestEvent;
import org.glassfish.jersey.server.monitoring.RequestEventListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Logger for operations performed on the Jersey Application.
 * 
 * @author Cedrick Lunven (@clunven)</a>
 */
@Provider
public class JerseyApplicationEventListener implements ApplicationEventListener {

    /** logger. */
    protected Logger logger = LoggerFactory.getLogger(getClass());
    
    @Override
    public void onEvent(ApplicationEvent appEvent) {
        switch(appEvent.getType()) {
            case INITIALIZATION_START:
                logger.info("Application Initialization");
            break;
            case INITIALIZATION_FINISHED:
                logger.info("Initialization done");
            break;
            case INITIALIZATION_APP_FINISHED:
                logger.info("Application is initialized");
            break;
            default:
            break;
        }
    }

    /** {@inheritDoc} */
    @Override
    public RequestEventListener onRequest(RequestEvent reEvent) {
        return new JerseyRequestEventListener();
    }

}
