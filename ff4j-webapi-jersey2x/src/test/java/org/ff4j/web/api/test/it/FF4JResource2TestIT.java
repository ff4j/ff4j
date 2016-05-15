package org.ff4j.web.api.test.it;

import javax.ws.rs.client.Entity;
import javax.ws.rs.core.Form;

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

import static org.ff4j.test.TestsFf4jConstants.*;
import static org.ff4j.web.FF4jWebConstants.*;

/**
 * Test core web resources /ff4j
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4JResource2TestIT extends AbstractWebResourceTestIT {

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
        Assert.assertTrue(resHttp.readEntity(String.class).contains("uptime"));
    }

    /**
     * TDD.
     */
    @Test
    public void testPostIsNotFlipped() {
        // Given
        assertFF4J.assertThatFeatureExist(F4);
        ff4j.disable(F4);
        assertFF4J.assertThatFeatureNotFlipped(F4);
        
        // When
        Response resHttp = resourceff4j().path(OPERATION_CHECK).path(F4)//
                // content-type
                .request(MediaType.APPLICATION_FORM_URLENCODED_TYPE)
                // Accept : Text Plain
                .accept(MediaType.TEXT_PLAIN_TYPE)
                // Method and post URL ENCODED
                .post(Entity.form(new Form()));
        
        // Then
        String resEntity = resHttp.readEntity(String.class);
        Assert.assertEquals("Expected status is 200", Status.OK.getStatusCode(), resHttp.getStatus());
        Assert.assertNotNull(resEntity);
        Assert.assertFalse(Boolean.valueOf(resEntity));
    }
    
    /**
     * TDD.
     */
    @Test
    public void testPostIsNotFlippedGET() {
        // Given
        assertFF4J.assertThatFeatureExist(F4);
        ff4j.disable(F4);
        assertFF4J.assertThatFeatureNotFlipped(F4);
        
        // When
        Response resHttp = resourceff4j() //
                .path(OPERATION_CHECK).path(F4) //
                .request().get(Response.class);
        
        // Then
        String resEntity = resHttp.readEntity(String.class);
        Assert.assertEquals("Expected status is 200", Status.OK.getStatusCode(), resHttp.getStatus());
        Assert.assertNotNull(resEntity);
        Assert.assertFalse(Boolean.valueOf(resEntity));
    }

    /**
     * TDD.
     */
    @Test
    public void testPostIsFlipped() {
        // Given
        assertFF4J.assertThatFeatureExist(F2);
        ff4j.getFeatureStore().enable(F2);
        assertFF4J.assertThatFeatureExist(F4);
        ff4j.getFeatureStore().enable(F4);
        assertFF4J.assertThatFeatureFlipped(F4);
        // When
        Form formData = new Form();
        formData.param(POST_PARAMNAME_FEATURE_UID, F4);
        Response resHttp = resourceff4j().path(OPERATION_CHECK).path(F4)
                // content-type
                .request(MediaType.APPLICATION_FORM_URLENCODED_TYPE)
                // Accept : Text Plain
                .accept(MediaType.TEXT_PLAIN_TYPE)
                // Method and post URL ENCODED
                .post(Entity.form(new Form()));
        
        // Then
        String resEntity = resHttp.readEntity(String.class);
        Assert.assertEquals("Expected status is 200", Status.OK.getStatusCode(), resHttp.getStatus());
        Assert.assertNotNull(resEntity);
        Assert.assertTrue(Boolean.valueOf(resEntity));
    }

    /**
     * TDD.
     */
    @Test
    public void testPostIsFlippedNotFound() {
        // Given
        assertFF4J.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
        // When
        Response resHttp = resourceff4j().path(OPERATION_CHECK).path(F_DOESNOTEXIST).request().get(Response.class);
        String resEntity = resHttp.readEntity(String.class);
        // Then
        Assert.assertEquals("Expected status is 404", Status.NOT_FOUND.getStatusCode(), resHttp.getStatus());
        Assert.assertNotNull(resEntity);
        Assert.assertTrue("Invalid error message : " + resEntity, resEntity.contains("not exist"));
    }

    /**
     * TDD.
     */
    @Test
    public void testPostIsFlippedInvalidParameter() {
        // Given
        assertFF4J.assertThatFeatureExist(AWESOME);
        // When
        Form formData = new Form();
        formData.param("InvalidParameter", "localhost");
        Response resHttp = resourceff4j().path(OPERATION_CHECK) //
                .path(AWESOME) //
                // content-type
                .request(MediaType.APPLICATION_FORM_URLENCODED_TYPE)
                // Accept : Text Plain
                .accept(MediaType.TEXT_PLAIN_TYPE)
                // Method and post URL ENCODED
                .post(Entity.form(new Form()));
        String resEntity = resHttp.readEntity(String.class);
        // Then
        Assert.assertEquals("Expected status is 400", Status.BAD_REQUEST.getStatusCode(), resHttp.getStatus());
        Assert.assertNotNull(resEntity);
        Assert.assertTrue(resEntity.contains("Invalid parameter"));
    }

    /**
     * TDD.
     */
    @Test
    public void testPostIsFlippedWithParameters() {
        // Given
        assertFF4J.assertThatFeatureExist(AWESOME);
        // When
        Form formData = new Form();
        formData.param("clientHostName", "localhost");
        Response resHttp = resourceff4j().path(OPERATION_CHECK) //
                .path(AWESOME) //
                // content-type
                .request(MediaType.APPLICATION_FORM_URLENCODED_TYPE)
                // Accept : Text Plain
                .accept(MediaType.TEXT_PLAIN_TYPE)
                // Method and post URL ENCODED
                .post(Entity.form(formData));
        String resEntity = resHttp.readEntity(String.class);
        
        // Then
        Assert.assertEquals("Expected status is 200", Status.OK.getStatusCode(), resHttp.getStatus());
        Assert.assertNotNull(resEntity);
        Assert.assertFalse(Boolean.valueOf(resEntity));
    } 

}
