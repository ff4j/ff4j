package org.ff4j.v1.web;

import org.ff4j.v1.FF4j;

/**
 * Helper to build an instance of {@link ApiConfig} and initialize the REST API.
 * 
 * @author Cedrick Lunven (@clunven)</a>
 */
public class ApiConfigBuilder {
    
    /** API Configuration. */
    private ApiConfig apiConfig;
    
    public ApiConfigBuilder() {
        apiConfig = new ApiConfig();
    }
    
    public ApiConfigBuilder(FF4j ff4j) {
        apiConfig = new ApiConfig(ff4j);
    }
    
    public ApiConfigBuilder(ApiConfig initConf) {
        apiConfig = initConf;
    }
    
    public ApiConfig build() {
        return apiConfig;
    }
    
    // Documentation
    
    public ApiConfigBuilder documentation(boolean b) {
        apiConfig.setDocumentation(b);
        return this;
    }
   
    public ApiConfigBuilder withDocumentation() {
        return documentation(true);
    }
    
    public ApiConfigBuilder withoutDocumentation() {
        return documentation(false);
    }
    
    // Authentication
    
    public ApiConfigBuilder authenticate(boolean b) {
        apiConfig.setAuthenticate(b);
        return this;
    }
    
    public ApiConfigBuilder withAuthentication() {
       return authenticate(true);
    }
    
    public ApiConfigBuilder withoutAuthentication() {
       return authenticate(false);
    }
    
    // Autorisation
    
    public ApiConfigBuilder autorize(boolean b) {
        apiConfig.setAutorize(b);
        return this;
    }
    
    public ApiConfigBuilder withAutorization() {
       return autorize(true);
    }
    
    public ApiConfigBuilder withoutAutorization() {
       return autorize(false);
    }
    
    public ApiConfigBuilder addApiKey(String apiKey) {
        apiConfig.createApiKey(apiKey, true, true, null);
        return this;
    }
    
    public ApiConfigBuilder addUser(String userName, String userPassword) {
        apiConfig.createUser(userName, userPassword, true, true, null);
        return this;
    }
    
    public ApiConfigBuilder host(String host) {
        apiConfig.setHost(host);
        return this;
    }
    
    public ApiConfigBuilder webContext(String context) {
        apiConfig.setWebContext(context);
        return this;
    }
    
    public ApiConfigBuilder port(int port) {
        apiConfig.setPort(port);
        return this;
    }
    

}
