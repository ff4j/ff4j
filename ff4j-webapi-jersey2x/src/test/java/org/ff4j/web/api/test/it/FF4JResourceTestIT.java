package org.ff4j.web.api.test.it;

/*
 * #%L
 * ff4j-web
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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

import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.ff4j.store.InMemoryFeatureStore;
import org.junit.Assert;
import org.junit.Test;

/**
 * Test core web resources /ff4j
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4JResourceTestIT extends AbstractWebResourceTestIT {

    /**
     * TDD.
     */
    @Test
    public void testGet() {
        // Given
        Assert.assertEquals(InMemoryFeatureStore.class, ff4j.getFeatureStore().getClass());
        // When
        Response resHttp = resourceff4j().request(MediaType.APPLICATION_JSON).get(Response.class);
        
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 200", Status.OK.getStatusCode(), resHttp.getStatus());
        
        // Then, Entity Object
        //Assert.assertTrue(resEntity.contains("uptime"));
    }

    /**
     * TDD.
     *
    @Test
    public void testPost_isNotFlipped() {
        // Given
        assertFF4J.assertThatFeatureExist(F4);
        assertFF4J.assertThatFeatureNotFlipped(F4);
        // When
        MultivaluedMap<String, String> formData = new MultivaluedMapImpl();
        //formData.add("", F4);
        Response resHttp = resourceff4j().path(OPERATION_CHECK).path(F4).//
                type(MediaType.APPLICATION_FORM_URLENCODED).//
                post(Response.class, formData);
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
    public void testPost_isNotFlippedGET() {
        // Given
        assertFF4J.assertThatFeatureExist(F4);
        ff4j.disable(F4);
        assertFF4J.assertThatFeatureNotFlipped(F4);
        // When
        Response resHttp = resourceff4j().path(OPERATION_CHECK).path(F4).//
                get(Response.class);
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
    public void testPost_isFlipped() {
        // Given
        assertFF4J.assertThatFeatureExist(F2);
        ff4j.getFeatureStore().enable(F2);
        assertFF4J.assertThatFeatureExist(F4);
        ff4j.getFeatureStore().enable(F4);
        assertFF4J.assertThatFeatureFlipped(F4);
        // When
        MultivaluedMap<String, String> formData = new MultivaluedMapImpl();
        formData.add(POST_PARAMNAME_FEATURE_UID, F4);
        Response resHttp = resourceff4j().path(OPERATION_CHECK).path(F4).//
                type(MediaType.APPLICATION_FORM_URLENCODED).//
                post(Response.class, formData);
        String resEntity = resHttp.getEntity(String.class);
        // Then
        Assert.assertEquals("Expected status is 200", Status.OK.getStatusCode(), resHttp.getStatus());
        Assert.assertNotNull(resEntity);
        Assert.assertTrue(Boolean.valueOf(resEntity));
    }

    /**
     * TDD.
     *
    @Test
    public void testPost_isFlippedNotFound() {
        // Given
        assertFF4J.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
        // When
        Response resHttp = resourceff4j().path(OPERATION_CHECK).path(F_DOESNOTEXIST).get(Response.class);
        String resEntity = resHttp.getEntity(String.class);
        // Then
        Assert.assertEquals("Expected status is 404", Status.NOT_FOUND.getStatusCode(), resHttp.getStatus());
        Assert.assertNotNull(resEntity);
        Assert.assertTrue("Invalid error message : " + resEntity, resEntity.contains("not exist"));
    }

    /**
     * TDD.
     *
    @Test
    public void testPost_isFlippedInvalidParameter() {
        // Given
        assertFF4J.assertThatFeatureExist(AWESOME);
        // When
        MultivaluedMap<String, String> formData = new MultivaluedMapImpl();
        formData.add("InvalidParameter", "localhost");
        Response resHttp = resourceff4j().path(OPERATION_CHECK) //
                .path(AWESOME) //
                .type(MediaType.APPLICATION_FORM_URLENCODED).//
                post(Response.class, formData);
        String resEntity = resHttp.getEntity(String.class);
        // Then
        Assert.assertEquals("Expected status is 400", Status.BAD_REQUEST.getStatusCode(), resHttp.getStatus());
        Assert.assertNotNull(resEntity);
        Assert.assertTrue(resEntity.contains("Invalid parameter"));
    }

    /**
     * TDD.
     *
    @Test
    public void testPost_isFlippedWithParameters() {
        // Given
        assertFF4J.assertThatFeatureExist(AWESOME);
        // When
        MultivaluedMap<String, String> formData = new MultivaluedMapImpl();
        formData.add("clientHostName", "localhost");
        Response resHttp = resourceff4j().path(OPERATION_CHECK) //
                .path(AWESOME) //
                .type(MediaType.APPLICATION_FORM_URLENCODED).//
                post(Response.class, formData);
        String resEntity = resHttp.getEntity(String.class);
        
        // Then
        Assert.assertEquals("Expected status is 200", Status.OK.getStatusCode(), resHttp.getStatus());
        Assert.assertNotNull(resEntity);
        Assert.assertFalse(Boolean.valueOf(resEntity));
    }  */  

}
