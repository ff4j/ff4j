package org.ff4j.web.api.utils;

/*
 * #%L
 * ff4j-webapi-jersey2x
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

import static org.ff4j.web.FF4jWebConstants.HEADER_AUTHORIZATION;
import static org.ff4j.web.FF4jWebConstants.PARAM_AUTHKEY;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import org.ff4j.utils.Util;
import org.ff4j.web.api.FF4jJacksonMapper;
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.internal.util.Base64;

import io.swagger.jaxrs.json.JacksonJsonProvider;

/**
 * Mutualization of HTTP METHODS.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class ClientHttpUtils {
    
    /**
     * Hide default constructor.
     */
    private ClientHttpUtils() {
    }
    
    /**
     * Initializing jerseyClient.
     */
    public static Client buildJerseyClient() {
        ClientConfig clientConfig = new ClientConfig();
        clientConfig.register(JacksonJsonProvider.class);
        clientConfig.register(FF4jJacksonMapper.class);
        return ClientBuilder.newClient(clientConfig);
    }
    
    /**
     * Put authentication header if relevant.
     *
     * @param webTarget
     * @param auth
     * @return
     */
    public static Invocation.Builder createRequest(WebTarget webTarget, String auth, MediaType mediaType) {
        Util.assertNotNull(webTarget);
        Invocation.Builder invocationBuilder = null;
        if (mediaType != null) {
            invocationBuilder = webTarget.request();
        } else {
            invocationBuilder = webTarget.request(mediaType);
        }
        if (Util.hasLength(auth)) {
            invocationBuilder.header(HEADER_AUTHORIZATION, auth);
        }
        return invocationBuilder;
    }
    
    /**
     * Share header settings for invocations.
     *
     * @param webTarget target web
     * @return
     */
    public static Response invokeGetMethod(WebTarget webTarget, String auth) {
        return createRequest(webTarget, auth, MediaType.APPLICATION_JSON_TYPE).get();
    }
    
    /**
     * Share header settings for invocations.
     *
     * @param webTarget target web
     * @return
     */
    public static Response invokeDeleteMethod(WebTarget webTarget, String auth) {
        return createRequest(webTarget, auth, null).delete();
    }
    
    /**
     * Share header settings for invocations.
     *
     * @param webTarget
     *          target web
     * @return
     */
    public static Response invokePostMethod(WebTarget webTarget, String auth) {
        return createRequest(webTarget, auth, MediaType.APPLICATION_JSON_TYPE).post(Entity.text(""));
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
        return " Basic " + new String(Base64.encodeAsString(username + ":" + password));
    }
    
}