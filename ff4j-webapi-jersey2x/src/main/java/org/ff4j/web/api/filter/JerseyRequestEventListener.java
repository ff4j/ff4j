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
    
    public JerseyRequestEventListener() {
    }
	
    @Override
	public void onEvent(RequestEvent reqEvt) {
    	switch(reqEvt.getType()) {
    		case MATCHING_START:
    			logger.debug(reqEvt.getContainerRequest().getRequestUri() + "[Matching]");
			break;
    		case REQUEST_MATCHED:
    		    logger.debug(reqEvt.getContainerRequest().getRequestUri() + "[Matched]");
            break;
    	    case REQUEST_FILTERED:
    	        logger.debug(reqEvt.getContainerRequest().getRequestUri() + "[Filters]");
            break;
    		case RESOURCE_METHOD_START:
    		    logger.debug(reqEvt.getContainerRequest().getRequestUri() + "[START]");
            break;
    		
    		case RESOURCE_METHOD_FINISHED:
    		    logger.debug(reqEvt.getContainerRequest().getRequestUri() + "[START]");
			break;
    		
			case EXCEPTION_MAPPING_FINISHED:
				logger.info(reqEvt.getType() + ") 8° Exception Mapping End");
			break;
			case RESP_FILTERS_START:
                logger.info(reqEvt.getType() + ") 6° Filter Start");
            break;
            case RESP_FILTERS_FINISHED:
                logger.info(reqEvt.getType() + ") 7° Filter End");
            break;
			case FINISHED:
				logger.info(reqEvt.getType() + ") 9° Finished");
			break;
			case LOCATOR_MATCHED:
				logger.info(reqEvt.getType() + ") 10° Locator");
				break;
			case ON_EXCEPTION:
				logger.error("An error occured when processing " + reqEvt.getContainerRequest().getRequestUri(), reqEvt.getException() );
				logger.error(" + Type : " + reqEvt.getException().getClass().getCanonicalName());
				logger.error(" + Message : " + reqEvt.getException().getMessage());
			break;
			case EXCEPTION_MAPPER_FOUND:
				logger.info(reqEvt.getType() + ") 12° Exception Mapper");
			break;
			case START:
				logger.info(reqEvt.getType() + ") 13° ON Start");
			break;
			case SUBRESOURCE_LOCATED:
				logger.info(reqEvt.getType() + ") 14° Subresource");
				break;
			default:
			    logger.debug(reqEvt.getContainerRequest().getRequestUri() + "[" + reqEvt.getType() + "]");
				break;
	    	}
	}

}
