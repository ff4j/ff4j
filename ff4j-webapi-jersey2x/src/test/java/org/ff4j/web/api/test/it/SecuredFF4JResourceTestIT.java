package org.ff4j.web.api.test.it;

import static org.ff4j.test.TestsFf4jConstants.F4;
import static org.ff4j.test.TestsFf4jConstants.TEST_FEATURES_FILE;
import static org.ff4j.web.FF4jWebConstants.HEADER_AUTHORIZATION;
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
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Application;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.ff4j.FF4j;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.test.AssertFf4j;
import org.ff4j.web.api.FF4jJacksonMapper;
import org.ff4j.web.api.resources.FF4jResource;
import org.ff4j.web.api.security.FF4JSecurityContextAuthenticationManager;
import org.ff4j.web.api.utils.ClientHttpUtils;
import org.ff4j.web.jersey2.store.FeatureStoreHttp;
import org.glassfish.jersey.test.grizzly.GrizzlyTestContainerFactory;
import org.glassfish.jersey.test.spi.TestContainerFactory;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Force security through API KEY and check.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class SecuredFF4JResourceTestIT  extends AbstractWebResourceTestIT {
    
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
        super.setUp();
        // Bridge security between ff4j and jersey
        ff4j.setAuthorizationsManager(new FF4JSecurityContextAuthenticationManager());
        if (assertFF4J == null) {
            assertFF4J = new AssertFf4j(ff4j);
        }
    }
    
    @Override
    protected Application configure() {
        return new SecuredJersey2Application(ff4j);
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
    

    /** {@inheritDoc} */
    @Override
    public TestContainerFactory getTestContainerFactory() {
        return new GrizzlyTestContainerFactory();
    }
    
    /**
     * Convenient method to get a resource for {@link FF4jResource}
     * 
     * @return web resource
     */
    protected WebTarget resourceff4j() {
        return target().path(APIPATH);
    }

    /**
     * Convenient method to get a resource for {@link FeatureStoreHttp}
     * 
     * @return web resource
     */
    protected WebTarget resourceStore() {
        return resourceff4j().path(RESOURCE_STORE);
    }

    /**
     * Convenient method to get a resource for {@link FeaturesResource}
     * 
     * @return web resource
     */
    protected WebTarget resourceFeatures() {
        return resourceStore().path(RESOURCE_FEATURES);
    }

    /**
     * Convenient method to get a resource for {@link GroupsResource}
     * 
     * @return web resource
     */
    protected WebTarget resourceGroups() {
        return resourceStore().path(RESOURCE_GROUPS);
    }
    
    /**
     * TDD.
     */
    @Test
    public void testKONotAuthorizedNoApiKeyNorCredentials() {
        // Given
        Assert.assertEquals(InMemoryFeatureStore.class, ff4j.getFeatureStore().getClass());
        // When
        Response resHttp = resourceff4j().request(MediaType.APPLICATION_JSON).get();
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 401", Status.UNAUTHORIZED.getStatusCode(), resHttp.getStatus());
    }
    
    /**
     * TDD.
     */
    @Test
    public void testOKWithApiKey() {
        // Given
        Assert.assertEquals(InMemoryFeatureStore.class, ff4j.getFeatureStore().getClass());
        // When
        Response resHttp = resourceff4j() //
                .request(MediaType.APPLICATION_JSON)
                .header(HEADER_AUTHORIZATION, PARAM_AUTHKEY + "=456" ).get();
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 200", Status.OK.getStatusCode(), resHttp.getStatus());
    }
    
    /**
     * TDD.
     */
    @Test
    public void testKOWithInvalidApiKey() {
        // Given
        Assert.assertEquals(InMemoryFeatureStore.class, ff4j.getFeatureStore().getClass());
        // When
        Response resHttp = resourceff4j() //
                .request(MediaType.APPLICATION_JSON)
                .header(HEADER_AUTHORIZATION, PARAM_AUTHKEY + "=INVALID" ).get();
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 401", Status.UNAUTHORIZED.getStatusCode(), resHttp.getStatus());
    }
    
    /**
     * TDD.
     */
    @Test
    public void testOKWithCredentials() {
        // Given
        Assert.assertEquals(InMemoryFeatureStore.class, ff4j.getFeatureStore().getClass());
        // When
        String authent = ClientHttpUtils.buildAuthorization4UserName("user", "user");
        Response resHttp = resourceff4j() //
                .request(MediaType.APPLICATION_JSON)
                .header(HEADER_AUTHORIZATION, authent).get();
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 200", Status.OK.getStatusCode(), resHttp.getStatus());
    }
    
    /**
     * TDD.
     */
    @Test
    public void testKOWithInvalidUserName() {
        // Given
        Assert.assertEquals(InMemoryFeatureStore.class, ff4j.getFeatureStore().getClass());
        // When
        String authent = ClientHttpUtils.buildAuthorization4UserName("incalidUser", "user");
        Response resHttp = resourceff4j() //
                .request(MediaType.APPLICATION_JSON)
                .header(HEADER_AUTHORIZATION, authent).get();
        
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 401", Status.UNAUTHORIZED.getStatusCode(), resHttp.getStatus());
    }
    
    /**
     * TDD.
     */
    @Test
    public void testKOWithInvalidPassword() {
        // Given
        Assert.assertEquals(InMemoryFeatureStore.class, ff4j.getFeatureStore().getClass());
        // When
        String authent = ClientHttpUtils.buildAuthorization4UserName("user", "invalidPassword");
        Response resHttp = resourceff4j() //
                .request(MediaType.APPLICATION_JSON)
                .header(HEADER_AUTHORIZATION, authent).get();
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 401", Status.UNAUTHORIZED.getStatusCode(), resHttp.getStatus());
    }
    
    /**
     * TDD.
     * 
     * Authorization filter is not used it all unit testing launch at once :(
     */
    @Test
    @Ignore
    public void testKOWithReadOnlyApiKey() {
        // Given
        assertFF4J.assertThatFeatureExist(F4);
        assertFF4J.assertThatFeatureIsEnabled(F4);
        // When
        Response resHttp = resourceFeatures().path(F4).path(OPERATION_DISABLE)
                .request(MediaType.APPLICATION_JSON)
                .header(HEADER_AUTHORIZATION, PARAM_AUTHKEY + "=123" ).post(Entity.text(""));
        
         Assert.assertEquals("Expected status is FORBIDDEN", Status.FORBIDDEN.getStatusCode(), resHttp.getStatus());
    }
    
    /**
     * TDD.
     *
    @Test
    public void testBridgeSecurityContext_PermissionDenied() {
        // Given
        assertFF4J.assertThatFeatureExist(F1);
        assertFF4J.assertThatFeatureIsEnabled(F1);
        // When
        ClientResponse resHttp = resourceff4j().path(OPERATION_CHECK).path(F1).//
                type(MediaType.APPLICATION_JSON).//
                header(HEADER_AUTHORIZATION, FeatureStoreHttp.buildAuthorization4UserName("user", "user")). //
                get(ClientResponse.class);
        
        String resEntity = resHttp.getEntity(String.class);
        // Then
        Assert.assertEquals("Expected status is 200", Status.OK.getStatusCode(), resHttp.getStatus());
        Assert.assertNotNull(resEntity);
        Assert.assertFalse(Boolean.valueOf(resEntity));
    }
    
    /**
     * TDD.
     *
    @Test
    public void testBridgeSecurityContext_PermissionGranted() {
        // Given
        assertFF4J.assertThatFeatureExist(F1);
        assertFF4J.assertThatFeatureIsEnabled(F1);
        // When
        ClientResponse resHttp = 
                resourceff4j().path(OPERATION_CHECK).path(F1).//
                type(MediaType.APPLICATION_JSON).//
                header(HEADER_AUTHORIZATION, FeatureStoreHttp.buildAuthorization4UserName("admin", "admin")). //
                get(ClientResponse.class);
        
        String resEntity = resHttp.getEntity(String.class);
        // Then
        Assert.assertEquals("Expected status is 200", Status.OK.getStatusCode(), resHttp.getStatus());
        Assert.assertNotNull(resEntity);
        Assert.assertTrue(Boolean.valueOf(resEntity));
    }*/


}
