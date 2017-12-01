package org.ff4j.web.api.security;

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
