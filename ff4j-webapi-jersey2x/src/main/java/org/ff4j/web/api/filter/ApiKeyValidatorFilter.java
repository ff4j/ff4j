package org.ff4j.web.api.filter;

import java.io.IOException;
import java.util.Map;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.container.ContainerRequestFilter;
import javax.ws.rs.core.Response.Status;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * Filter request if not identified by API Key.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class ApiKeyValidatorFilter implements ContainerRequestFilter {
    
    /** Expected Header params. */
    private static final String HEADER_APIKEY = "X-FF4J-APIKEY";
    
    /** logger. */
    protected Logger logger = LoggerFactory.getLogger(getClass());
    
    /** UserId, api Key. */
    private static Map < String, ApiKey > validApiKeysMap;
    
    /**
     * Instanciation through introspection
     */
    public ApiKeyValidatorFilter() {
    }
    
    /**
     * Initialization of apiKeys (from database for instance).
     *
     * @param apiKeys
     */
    public ApiKeyValidatorFilter(Map < String, ApiKey > apiKeys) {
        validApiKeysMap = apiKeys;
    }
	
    /**
     * Before Method invocation reading HTTP REQUEST.
     *
     * {@inheritDoc}
     */
	@Override
	public void filter(ContainerRequestContext reqCtx) throws IOException {
	    if (!reqCtx.getHeaders().containsKey(HEADER_APIKEY)) {
	        throw new WebApplicationException("API key (" + HEADER_APIKEY + ") is required for this API", Status.UNAUTHORIZED);
	    }
	    String apiKey = reqCtx.getHeaders().getFirst(HEADER_APIKEY);
	    if (!validApiKeysMap.containsKey(apiKey)) {
	        
	    }
	}

    /**
     * Getter accessor for attribute 'validApiKeysMap'.
     *
     * @return
     *       current value of 'validApiKeysMap'
     */
    public static Map<String, ApiKey> getValidApiKeysMap() {
        return validApiKeysMap;
    }

    /**
     * Setter accessor for attribute 'validApiKeysMap'.
     * @param validApiKeysMap
     * 		new value for 'validApiKeysMap '
     */
    public static void setValidApiKeysMap(Map<String, ApiKey> validApiKeysMap) {
        ApiKeyValidatorFilter.validApiKeysMap = validApiKeysMap;
    }

}
