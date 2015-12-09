package org.ff4j.web.api.filter;

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
    
    /**
     * Constructor to TODO
     */
    public JerseyRequestEventListener() {
    }
	
    /** {@inheritDoc} */
    @Override
	public void onEvent(RequestEvent reqEvt) {
    	switch(reqEvt.getType()) {
    	    // Exact Log of error within backend invocation
    		case ON_EXCEPTION:
				logger.error("An error occured when processing " + reqEvt.getContainerRequest().getRequestUri(), reqEvt.getException() );
				logger.error(" + Type : " + reqEvt.getException().getClass().getCanonicalName());
				logger.error(" + Message : " + reqEvt.getException().getMessage());
			break;
			default:
			    logger.debug(reqEvt.getContainerRequest().getRequestUri() + "[" + reqEvt.getType() + "]");
				break;
	    	}
	}

}
