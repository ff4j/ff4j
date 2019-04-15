package org.ff4j.web.api.utils;

import static org.ff4j.web.FF4jWebConstants.HEADER_AUTHORIZATION;
import static org.ff4j.web.FF4jWebConstants.PARAM_AUTHKEY;

import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.api.client.config.ClientConfig;
import com.sun.jersey.api.client.config.DefaultClientConfig;
import com.sun.jersey.api.json.JSONConfiguration;
import com.sun.jersey.core.util.Base64;

import javax.ws.rs.core.MediaType;

import org.ff4j.utils.Util;
import org.ff4j.web.api.FF4jJacksonMapper;

import io.swagger.jaxrs.json.JacksonJsonProvider;

public class ClientHttpUtils {

    /**
     * Hide default constructor.
     */
    private ClientHttpUtils() {
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

    public static WebResource.Builder createRequest(WebResource webResource, String auth, MediaType mediaType) {
        Util.assertNotNull(webResource);
        WebResource.Builder webResourceBuilder = mediaType != null
                ? webResource.type(mediaType)
                : webResource.getRequestBuilder();
        if (Util.hasLength(auth)) {
            webResourceBuilder.header(HEADER_AUTHORIZATION, auth);
        }
        return webResourceBuilder;
    }

    public static ClientResponse invokeGetMethod(WebResource webResource, String auth) {
        return createRequest(webResource, auth, null).get(ClientResponse.class);
    }

    public static ClientResponse invokePostMethod(WebResource webResource, String auth) {
        return createRequest(webResource, auth, null).post(ClientResponse.class);
    }

    public static ClientResponse invokePutMethod(WebResource webResource, String auth, Object requestEntity) {
        return createRequest(webResource, auth, MediaType.APPLICATION_JSON_TYPE).put(ClientResponse.class, requestEntity);
    }

    public static ClientResponse invokeDeleteMethod(WebResource webResource, String auth) {
        return createRequest(webResource, auth, null).delete(ClientResponse.class);
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
