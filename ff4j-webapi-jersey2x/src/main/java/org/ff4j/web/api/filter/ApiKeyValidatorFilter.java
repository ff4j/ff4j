package org.ff4j.web.api.filter;

/*
 * #%L
 * ff4j-webapi-jersey2x
 * %%
 * Copyright (C) 2013 - 2015 FF4J
 * %%
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */


import java.io.IOException;
import java.util.Date;
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
    public static final String HEADER_APIKEY = "X-FF4J-APIKEY";
    
    /** logger. */
    protected Logger logger = LoggerFactory.getLogger(getClass());
    
    /** UserId, api Key. */
    private static Map < String, ApiKey > validApiKeysMap;
    
    /**
     * Instantiation through introspection
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
        // Do not contains any API KEY
        if (!reqCtx.getHeaders().containsKey(HEADER_APIKEY)) {
            throw new WebApplicationException("API key (" + HEADER_APIKEY + ") is required for this API", Status.UNAUTHORIZED);
        }
        
        // Contains the header but invalid valid
        String apiKey = reqCtx.getHeaders().getFirst(HEADER_APIKEY);
        if (!validApiKeysMap.containsKey(apiKey)) {
            throw new WebApplicationException("Invalid API Key - not recognized", Status.UNAUTHORIZED);
        }
        
        // Does the API reach its expiration Date ?
        ApiKey currentKey = validApiKeysMap.get(apiKey);
        if (currentKey.getExpirationTime().before(new Date())) {
            throw new WebApplicationException("API key has expired ", Status.UNAUTHORIZED);
        }
        
        // from here : nothing to do, contains the header AND correct value AND still valid
        
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
     * @param validApiKeysMap new value for 'validApiKeysMap '
     */
    public static void setValidApiKeysMap(Map<String, ApiKey> validApiKeysMap) {
        ApiKeyValidatorFilter.validApiKeysMap = validApiKeysMap;
    }

}
