package org.ff4j.web.services.exception;

import javax.ws.rs.core.Response.Status;

import org.ff4j.exception.FeatureNotFoundException;

/**
 * Returning 404 HTTPCode if target feature has not been found.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureNotFoundExceptionMapper extends AbstractExceptionMapper<FeatureNotFoundException> {

    /** {@inheritdoc} */
    @Override
    public Status getStatus() {
        return Status.NOT_FOUND;
    }
}