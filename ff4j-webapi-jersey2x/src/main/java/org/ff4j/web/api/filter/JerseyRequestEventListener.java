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

import org.glassfish.jersey.server.monitoring.RequestEvent;
import org.glassfish.jersey.server.monitoring.RequestEventListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Event Listener for each request.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
@Provider
public class JerseyRequestEventListener implements RequestEventListener {

    /** logger. */
    protected Logger logger = LoggerFactory.getLogger(JerseyRequestEventListener.class);
   

    /** {@inheritDoc} */
    public void onEvent(RequestEvent reqEvt) {
        if (RequestEvent.Type.ON_EXCEPTION.equals(reqEvt.getType())) {
            logger.error("An error occured when processing " + reqEvt.getContainerRequest().getRequestUri(), reqEvt.getException() );
            logger.error(" + Type : " + reqEvt.getException().getClass().getName());
            logger.error(" + Message : " + reqEvt.getException().getMessage());
        } else {
            logger.debug(reqEvt.getContainerRequest().getRequestUri() + "[" + reqEvt.getType() + "]");
        }
    }

}
