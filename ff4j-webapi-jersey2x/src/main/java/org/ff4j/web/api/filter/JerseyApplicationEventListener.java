package org.ff4j.web.api.filter;

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
			case DESTROY_FINISHED:
			break;
			case RELOAD_FINISHED:
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
