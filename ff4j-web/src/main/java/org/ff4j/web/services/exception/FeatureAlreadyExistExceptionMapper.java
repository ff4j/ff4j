package org.ff4j.web.services.exception;

import javax.ws.rs.core.Response.Status;
import javax.ws.rs.ext.Provider;

import org.ff4j.exception.FeatureAlreadyExistException;

/**
 * HTTP Error conflict for already existing bean.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Provider
public class FeatureAlreadyExistExceptionMapper extends AbstractExceptionMapper<FeatureAlreadyExistException> {

    /** {@inheritdoc} */
    @Override
    public Status getStatus() {
        return Status.CONFLICT;
    }
}
