package org.ff4j.web.api.security;

/*
 * #%L
 * ff4j-webapi-jersey1x
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import static org.ff4j.web.FF4jWebConstants.PARAM_AUTHKEY;

import org.ff4j.web.api.FF4jJacksonMapper;

import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.config.ClientConfig;
import com.sun.jersey.api.client.config.DefaultClientConfig;
import com.sun.jersey.api.json.JSONConfiguration;
import com.sun.jersey.core.util.Base64;

import io.swagger.jaxrs.json.JacksonJsonProvider;

/**
 * Mutualization of security operations.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class ClientHttpJersey1Utils {
    
    /**
     * Hide default constructor.
     */
    private ClientHttpJersey1Utils() {
    }
    
    /**
     * Initializing jerseyClient.
     */
    public static Client buildJersey1Client() {
        ClientConfig config = new DefaultClientConfig();
        config.getFeatures().put(JSONConfiguration.FEATURE_POJO_MAPPING, Boolean.TRUE);
        config.getSingletons().add(new JacksonJsonProvider());
        config.getSingletons().add(new FF4jJacksonMapper());
        return Client.create(config);
    }
    
    /**
     * Build Authorization header for technical user.
     * @param apiKey target apiKey
     * @return target header
     */
    public static String buildAuthorization4ApiKey(String apiKey) {
        return PARAM_AUTHKEY + "=" + apiKey;
    }
    
    /**
     * Build Authorization header for final user.
     * @param username target username
     * @param password target password
     * @return target header
     */
    public static String buildAuthorization4UserName(String username, String password) {
        return " Basic " + new String(Base64.encode(username + ":" + password));
    }

}
