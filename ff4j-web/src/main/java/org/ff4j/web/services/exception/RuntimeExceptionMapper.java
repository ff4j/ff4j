package org.ff4j.web.services.exception;

import javax.ws.rs.core.Response.Status;
import javax.ws.rs.ext.Provider;

/**
 * Once Runtime Error occur, return 500 with only the error message
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Provider
public final class RuntimeExceptionMapper extends AbstractExceptionMapper<RuntimeException> {

    /** {@inheritdoc} */
    @Override
    public Status getStatus() {
        return Status.INTERNAL_SERVER_ERROR;
    }

}