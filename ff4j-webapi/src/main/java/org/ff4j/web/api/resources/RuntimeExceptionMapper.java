package org.ff4j.web.api.resources;

import javax.ws.rs.WebApplicationException;

/*
 * #%L ff4j-web %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
 * file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.ext.ExceptionMapper;
import javax.ws.rs.ext.Provider;

import org.ff4j.exception.FeatureAccessException;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.exception.PropertyAccessException;
import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.exception.PropertyNotFoundException;

/**
 * Encapsulation of exception during {@link FeatureWebService} accesses.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 * 
 * @param <T>
 *            target exception to trap
 */
@Provider
public class RuntimeExceptionMapper implements ExceptionMapper<RuntimeException> {

    /** Target exception message as String. */
    private static final String CONTENT_TYPE = "text/plain";

    /** {@inheritdoc} */
    @Override
    public Response toResponse(RuntimeException rex) {
        Status myStatus = getStatus(rex);
        return Response.status(myStatus).//
                entity(rex.getMessage()). //
                type(CONTENT_TYPE).build();
    }

    /**
     * Get HTTP Error code related to ff4j error.
     * 
     * @return http error code
     */
    public Status getStatus(RuntimeException rex) {
        if ((rex instanceof FeatureNotFoundException) || 
            (rex instanceof GroupNotFoundException) ||
            (rex instanceof PropertyNotFoundException)) {
            return Status.NOT_FOUND;
        }
        if ((rex instanceof FeatureAlreadyExistException) || 
            (rex instanceof PropertyAlreadyExistException)) {
            return Status.CONFLICT;
        }
        if ((rex instanceof FeatureAccessException) ||
            (rex instanceof PropertyAccessException)) {
            return Status.SERVICE_UNAVAILABLE;
        }
        // Propagation of existing code
        if (rex instanceof WebApplicationException) {
            return Status.fromStatusCode(((WebApplicationException) rex).getResponse().getStatus());
        }
        return Status.INTERNAL_SERVER_ERROR;
    }

}
