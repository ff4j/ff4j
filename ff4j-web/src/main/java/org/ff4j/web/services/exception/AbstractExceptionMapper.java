package org.ff4j.web.services.exception;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.ext.ExceptionMapper;

import org.ff4j.web.services.FeatureWebService;

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
