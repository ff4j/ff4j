package org.ff4j.web.services.exception;

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

/**
 * Encapsulation of exception during {@link FeatureWebService} accesses.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 * 
 * @param <T>
 *            target exception to trap
 */
public abstract class AbstractExceptionMapper<T extends Throwable> implements ExceptionMapper<T> {

    /** Target exception message as String. */
    private static final String CONTENT_TYPE = "text/plain";

    /** {@inheritdoc} */
    @Override
    public Response toResponse(T rex) {
        return Response.status(getStatus()).entity(rex.getMessage()).type(CONTENT_TYPE).build();
    }

    /**
     * Get HTTP Error code related to ff4j error.
     * 
     * @return http error code
     */
    protected abstract Status getStatus();

}
