package org.ff4j.web;

/*
 * #%L
 * ff4j-core
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


import org.ff4j.FF4j;

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
