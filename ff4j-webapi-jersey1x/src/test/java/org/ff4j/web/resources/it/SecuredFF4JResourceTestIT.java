package org.ff4j.web.resources.it;

import static org.ff4j.test.TestsFf4jConstants.F1;
import static org.ff4j.test.TestsFf4jConstants.F4;
import static org.ff4j.test.TestsFf4jConstants.TEST_FEATURES_FILE;
import static org.ff4j.web.FF4jWebConstants.HEADER_AUTHORIZATION;
import static org.ff4j.web.FF4jWebConstants.OPERATION_CHECK;
import static org.ff4j.web.FF4jWebConstants.OPERATION_DISABLE;
import static org.ff4j.web.FF4jWebConstants.PARAM_AUTHKEY;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_FEATURES;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_GROUPS;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_STORE;

/*
 * #%L
 * ff4j-web
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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

import javax.ws.rs.Path;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response.Status;

import org.ff4j.FF4j;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.test.AssertFf4j;
import org.ff4j.utils.Util;
import org.ff4j.web.ApiConfig;
import org.ff4j.web.ApiConfigBuilder;
import org.ff4j.web.api.FF4JApiApplication;
import org.ff4j.web.api.FF4jJacksonMapper;
import org.ff4j.web.api.resources.FF4jResource;
import org.ff4j.web.api.security.ClientHttpJersey1Utils;
import org.ff4j.web.api.security.FF4JSecurityContextAuthenticationManager;
import org.ff4j.web.jersey1.store.FeatureStoreHttp;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.api.client.config.ClientConfig;
import com.sun.jersey.api.client.config.DefaultClientConfig;
import com.sun.jersey.api.json.JSONConfiguration;
import com.sun.jersey.spi.container.servlet.WebComponent;
import com.sun.jersey.test.framework.JerseyTest;
import com.sun.jersey.test.framework.WebAppDescriptor;
import com.sun.jersey.test.framework.spi.container.TestContainerFactory;
import com.sun.jersey.test.framework.spi.container.grizzly2.web.GrizzlyWebTestContainerFactory;
/**
 * Force security through API KEY and check.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class SecuredFF4JResourceTestIT extends JerseyTest {
    
    /** Relative resource. */
    public final static String APIPATH = FF4jResource.class.getAnnotation(Path.class).value();

    /** Assert for this ff4j instance. */
    protected static AssertFf4j assertFF4J;

    /** Current ff4j. */
    protected static FF4j ff4j = new FF4j(TEST_FEATURES_FILE);
    
    /** Jackson serializer. */
    protected ObjectMapper jacksonMapper;
    
    /** {@inheritDoc} */
    @Override
    @Before
    public void setUp() throws Exception {
        // Bridge security between ff4j and jersey
        ff4j.setAuthorizationsManager(new FF4JSecurityContextAuthenticationManager());
        // <--
        
        if (assertFF4J == null) {
            assertFF4J = new AssertFf4j(ff4j);
        }
    }
    
    /**
     * Serialize with custom jackson.
     * @param o
     *      current object
     * @return
     *      serialize
     */
    protected String toJson(Object o) {
        try {
            if (jacksonMapper == null) {
                jacksonMapper = new FF4jJacksonMapper().getContext(getClass());
            }
            return jacksonMapper.writeValueAsString(o);
        } catch (Exception e) {
            throw new IllegalArgumentException("Cannot serialize", e);
        }
    }
    
    /**
     * Utilization of out-of-thr-box jersey configuration.
     *
     * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
     */
    public static class SecuredFF4jProvider extends FF4JApiApplication {

        /** {@inheritDoc} */
        @Override
        public ApiConfig getApiConfig() {
            ApiConfig secured = new ApiConfigBuilder(ff4j)//
                    .withAuthentication() //
                    .withAutorization().build();
            secured.createApiKey("123", true, false, Util.set("ROLE_USER", "ROLE_ADMIN"));
            secured.createApiKey("456", true, true, Util.set("ROLE_USER", "ROLE_ADMIN"));
            secured.createUser("user", "user", true, false, Util.set("ROLE_USER", "ROLE_ADMIN"));
            secured.createUser("admin", "admin", true, true, Util.set("ADMINISTRATOR", "USER"));
            return secured;
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public WebAppDescriptor configure() {
        ClientConfig cc = new DefaultClientConfig();
        cc.getFeatures().put(JSONConfiguration.FEATURE_POJO_MAPPING, Boolean.TRUE);
        return new WebAppDescriptor.Builder()
                .initParam(WebComponent.APPLICATION_CONFIG_CLASS, SecuredFF4jProvider.class.getName())//
                .clientConfig(cc).build();
    }

    /** {@inheritDoc} */
    @Override
    public TestContainerFactory getTestContainerFactory() {
        return new GrizzlyWebTestContainerFactory();
    }
    
    /**
     * Convenient method to get a resource for {@link FF4jResource}
     * 
     * @return web resource
     */
    protected WebResource resourceff4j() {
        return resource().path(APIPATH);
    }

    /**
     * Convenient method to get a resource for {@link FeatureStoreHttp}
     * 
     * @return web resource
     */
    protected WebResource resourceStore() {
        return resourceff4j().path(RESOURCE_STORE);
    }

    /**
     * Convenient method to get a resource for {@link FeaturesResource}
     * 
     * @return web resource
     */
    protected WebResource resourceFeatures() {
        return resourceStore().path(RESOURCE_FEATURES);
    }

    /**
     * Convenient method to get a resource for {@link GroupsResource}
     * 
     * @return web resource
     */
    protected WebResource resourceGroups() {
        return resourceStore().path(RESOURCE_GROUPS);
    }
    
    /**
     * TDD.
     */
    @Test
    public void testKO_NotAuthorized_NoApiKeyNorCredentials() {
        // Given
        Assert.assertEquals(InMemoryFeatureStore.class, ff4j.getFeatureStore().getClass());
        // When
        ClientResponse resHttp = resourceff4j().type(MediaType.APPLICATION_JSON).get(ClientResponse.class);
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 401", Status.UNAUTHORIZED.getStatusCode(), resHttp.getStatus());
    }
    
    /**
     * TDD.
     */
    @Test
    public void testOK_withApiKey() {
        // Given
        Assert.assertEquals(InMemoryFeatureStore.class, ff4j.getFeatureStore().getClass());
        // When
        ClientResponse resHttp = resourceff4j().header(HEADER_AUTHORIZATION, PARAM_AUTHKEY + "=456" ).type(MediaType.APPLICATION_JSON).get(ClientResponse.class);
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 200", Status.OK.getStatusCode(), resHttp.getStatus());
    }
    
    /**
     * TDD.
     */
    @Test
    public void testOK_withCredentials() {
        // Given
        Assert.assertEquals(InMemoryFeatureStore.class, ff4j.getFeatureStore().getClass());
        // When
        String authent = ClientHttpJersey1Utils.buildAuthorization4UserName("user", "user");
        ClientResponse resHttp = resourceff4j().header(HEADER_AUTHORIZATION, authent).type(MediaType.APPLICATION_JSON).get(ClientResponse.class);
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 200", Status.OK.getStatusCode(), resHttp.getStatus());
    }
    
    /**
     * TDD.
     */
    @Test
    public void testKO_withInvalidApiKey() {
        // Given
        Assert.assertEquals(InMemoryFeatureStore.class, ff4j.getFeatureStore().getClass());
        // When
        ClientResponse resHttp = resourceff4j().header(HEADER_AUTHORIZATION, PARAM_AUTHKEY + "=INVALID" ).type(MediaType.APPLICATION_JSON).get(ClientResponse.class);
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 401", Status.UNAUTHORIZED.getStatusCode(), resHttp.getStatus());
    }
    
    /**
     * TDD.
     */
    @Test
    public void testKO_withInvalidUserName() {
        // Given
        Assert.assertEquals(InMemoryFeatureStore.class, ff4j.getFeatureStore().getClass());
        // When
        String authent = ClientHttpJersey1Utils.buildAuthorization4UserName("incalidUser", "user");
        ClientResponse resHttp = resourceff4j().header(HEADER_AUTHORIZATION, authent).type(MediaType.APPLICATION_JSON).get(ClientResponse.class);
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 401", Status.UNAUTHORIZED.getStatusCode(), resHttp.getStatus());
    }
    
    /**
     * TDD.
     */
    @Test
    public void testKO_withInvalidPassword() {
        // Given
        Assert.assertEquals(InMemoryFeatureStore.class, ff4j.getFeatureStore().getClass());
        // When
        String authent = ClientHttpJersey1Utils.buildAuthorization4UserName("user", "invalidPassword");
        ClientResponse resHttp = resourceff4j().header(HEADER_AUTHORIZATION, authent).type(MediaType.APPLICATION_JSON).get(ClientResponse.class);
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 401", Status.UNAUTHORIZED.getStatusCode(), resHttp.getStatus());
    }
    
    /**
     * TDD.
     */
    @Test
    public void testKO_withReadOnlyApiKey() {
        // Given
        assertFF4J.assertThatFeatureExist(F4);
        assertFF4J.assertThatFeatureIsEnabled(F4);
        // When
        WebResource wResf4 = resourceFeatures().path(F4);
        ClientResponse resHttp = wResf4.path(OPERATION_DISABLE).header(HEADER_AUTHORIZATION, PARAM_AUTHKEY + "=123" ).type(MediaType.APPLICATION_JSON).post(ClientResponse.class);
        // Then, HTTPResponse
        // BUG JERSEY ==> return a 500 error inst
        //Assert.assertEquals("Expected status is 403", Status.FORBIDDEN.getStatusCode(), resHttp.getStatus());
        Assert.assertEquals("Expected status is FORBIDDEN", Status.FORBIDDEN.getStatusCode(), resHttp.getStatus());
    }
    
    /**
     * TDD.
     */
    @Test
    public void testBridgeSecurityContext_PermissionDenied() {
        // Given
        assertFF4J.assertThatFeatureExist(F1);
        assertFF4J.assertThatFeatureIsEnabled(F1);
        // When
        ClientResponse resHttp = resourceff4j().path(OPERATION_CHECK).path(F1).//
                type(MediaType.APPLICATION_JSON).//
                header(HEADER_AUTHORIZATION, ClientHttpJersey1Utils.buildAuthorization4UserName("user", "user")). //
                get(ClientResponse.class);
        
        String resEntity = resHttp.getEntity(String.class);
        // Then
        Assert.assertEquals("Expected status is 200", Status.OK.getStatusCode(), resHttp.getStatus());
        Assert.assertNotNull(resEntity);
        Assert.assertFalse(Boolean.valueOf(resEntity));
    }
    
    /**
     * TDD.
     */
    @Test
    public void testBridgeSecurityContext_PermissionGranted() {
        // Given
        assertFF4J.assertThatFeatureExist(F1);
        assertFF4J.assertThatFeatureIsEnabled(F1);
        // When
        ClientResponse resHttp = 
                resourceff4j().path(OPERATION_CHECK).path(F1).//
                type(MediaType.APPLICATION_JSON).//
                header(HEADER_AUTHORIZATION, ClientHttpJersey1Utils.buildAuthorization4UserName("admin", "admin")). //
                get(ClientResponse.class);
        
        String resEntity = resHttp.getEntity(String.class);
        // Then
        Assert.assertEquals("Expected status is 200", Status.OK.getStatusCode(), resHttp.getStatus());
        Assert.assertNotNull(resEntity);
        Assert.assertTrue(Boolean.valueOf(resEntity));
    }


}
