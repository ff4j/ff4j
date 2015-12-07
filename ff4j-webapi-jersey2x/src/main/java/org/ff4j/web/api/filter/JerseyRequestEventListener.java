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
    
    private String correlationID = null;
    
    public JerseyRequestEventListener() {
    }
	
    @Override
	public void onEvent(RequestEvent reqEvt) {
    	switch(reqEvt.getType()) {
    		case MATCHING_START:
    			logger.info("(1) Matching " + reqEvt.getUriInfo().getPath());
			break;
    		case REQUEST_MATCHED:
    			logger.info("(2) Matched");
			break;
    		case REQUEST_FILTERED:
    			logger.info(correlationID + " - " + reqEvt.getType() + ") 3° Filtered");
			break;
    		case RESOURCE_METHOD_START:
    			logger.info(correlationID + " - " + reqEvt.getType() + ") 4° Starting");
			break;
    		case RESOURCE_METHOD_FINISHED:
    			logger.info(correlationID + " - " + reqEvt.getType() + ") 5° Ending");
			break;
    		case RESP_FILTERS_START:
    			logger.info(correlationID + " - " + reqEvt.getType() + ") 6° Filter Start");
			break;
			case RESP_FILTERS_FINISHED:
				logger.info(correlationID + " - " + reqEvt.getType() + ") 7° Filter End");
			break;
			case EXCEPTION_MAPPING_FINISHED:
				logger.info(correlationID + " - " + reqEvt.getType() + ") 8° Exception Mapping End");
			break;
			case FINISHED:
				logger.info(correlationID + " - " + reqEvt.getType() + ") 9° Finished");
			break;
			case LOCATOR_MATCHED:
				logger.info(correlationID + " - " + reqEvt.getType() + ") 10° Locator");
				break;
			case ON_EXCEPTION:
				logger.info(correlationID + " - " + reqEvt.getType() + ") 11° ON Exception");
			break;
			case EXCEPTION_MAPPER_FOUND:
				logger.info(correlationID + " - " + reqEvt.getType() + ") 12° Exception Mapper");
			break;
			case START:
				logger.info(correlationID + " - " + reqEvt.getType() + ") 13° ON Start");
			break;
			case SUBRESOURCE_LOCATED:
				logger.info(correlationID + " - " + reqEvt.getType() + ") 14° Subresource");
				break;
			default:
				logger.info(correlationID + " - " + reqEvt.getType() + ") Default");
				break;
	    	}
	}

}
