package org.ff4j.web.api.test.embedded;

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

import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.WebTarget;

import org.ff4j.web.api.FF4jJacksonMapper;
import org.ff4j.web.api.test.SampleFF4jJersey2Application;
import org.glassfish.grizzly.http.server.HttpServer;
import org.glassfish.grizzly.http.server.NetworkListener;
import org.glassfish.grizzly.servlet.ServletRegistration;
import org.glassfish.grizzly.servlet.WebappContext;
import org.glassfish.jersey.client.ClientConfig;
import org.glassfish.jersey.servlet.ServletContainer;
import org.junit.After;
import org.junit.Before;
import org.junit.BeforeClass;

import io.swagger.jaxrs.json.JacksonJsonProvider;

public class AbstractEmbeddedGrizzlyIntegrationTest {
    
    private static final String HOST = "localhost";
    
    private static final int PORT = 3388;
    
    private static WebappContext webappContext;
    
    private static Client client = null;;
    
    private static HttpServer server = null;
    
    @BeforeClass
    public static void initOnce()  
    throws IOException {
        // WebContext for testing
        webappContext = new WebappContext("Test Context");
        ServletRegistration servletRegistration = webappContext.addServlet( "jersey-servlet", ServletContainer.class);
        servletRegistration.setInitParameter("com.sun.jersey.spi.container.ContainerResponseFilters", "com.sun.jersey.api.container.filter.LoggingFilter");
        servletRegistration.setInitParameter("com.sun.jersey.spi.container.ContainerRequestFilters",  "com.sun.jersey.api.container.filter.LoggingFilter");
        servletRegistration.setInitParameter("javax.ws.rs.Application", SampleFF4jJersey2Application.class.getCanonicalName());
        servletRegistration.setInitParameter("com.sun.jersey.api.json.POJOMappingFeature", "true");
        servletRegistration.addMapping("/*");
        
        ClientConfig clientConfig = new ClientConfig();
        clientConfig.register(JacksonJsonProvider.class);
        clientConfig.register(FF4jJacksonMapper.class);
        client = ClientBuilder.newClient(clientConfig);
        
        server = new HttpServer();
        NetworkListener listener = new NetworkListener("grizzly2", HOST, PORT);
        server.addListener(listener);
        webappContext.deploy(server);
        
    }
    
    protected WebTarget target() {
        return client.target("http://" + HOST + ":" + PORT);
    }
    
    @Before
    public void start() throws IOException {
        server.start();
    }
    
   
    
    @After
    public void stop() {
        server.shutdownNow();
    }

}
