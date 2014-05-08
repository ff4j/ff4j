package org.ff4j.web.resources.it;

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

import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.Response.Status;

import junit.framework.Assert;

import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.utils.FeatureJsonMarshaller;
import org.junit.Ignore;
import org.junit.Test;

import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.core.util.MultivaluedMapImpl;

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
    @Ignore
    public void testGet() {
        // Given
        Assert.assertTrue(ff4j.getStore() instanceof InMemoryFeatureStore);
        // When
        ClientResponse resHttp = resourceff4j().get(ClientResponse.class);
        String resEntity = resHttp.getEntity(String.class);
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 200", Status.OK.getStatusCode(), resHttp.getStatus());
        Assert.assertNotNull(resEntity);
        // Then, Entity Object
        Assert.assertTrue(resEntity.contains(InMemoryFeatureStore.class.getCanonicalName()));
    }

    /**
     * TDD.
     */
    @Test
    public void testPost_isNotFlipped() {
        // Given
        assertFF4J.assertThatFeatureExist(F4);
        assertFF4J.assertThatFeatureNotFlipped(F4);
        // When
        MultivaluedMap<String, String> formData = new MultivaluedMapImpl();
        formData.add(POST_PARAMNAME_FEATURE_UID, F4);
        ClientResponse resHttp = resourceff4j().path(OPERATION_FLIP).post(ClientResponse.class, formData);
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
    public void testPost_isFlipped() {
        // Given
        assertFF4J.assertThatFeatureExist(F2);
        ff4j.getStore().enable(F2);
        assertFF4J.assertThatFeatureExist(F4);
        assertFF4J.assertThatFeatureFlipped(F4);
        // When
        MultivaluedMap<String, String> formData = new MultivaluedMapImpl();
        formData.add(POST_PARAMNAME_FEATURE_UID, F4);
        ClientResponse resHttp = resourceff4j().path(OPERATION_FLIP).post(ClientResponse.class, formData);
        String resEntity = resHttp.getEntity(String.class);
        // Then
        Assert.assertEquals("Expected status is 200", Status.OK.getStatusCode(), resHttp.getStatus());
        Assert.assertNotNull(resEntity);
        Assert.assertTrue(Boolean.valueOf(resEntity));
    }

    /**
     * TDD.
     */
    @Test
    public void testPost_isFlippedNotFound() {
        // Given
        assertFF4J.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
        // When
        MultivaluedMap<String, String> formData = new MultivaluedMapImpl();
        formData.add(POST_PARAMNAME_FEATURE_UID, F_DOESNOTEXIST);
        ClientResponse resHttp = resourceff4j().path(OPERATION_FLIP).post(ClientResponse.class, formData);
        String resEntity = resHttp.getEntity(String.class);
        // Then
        Assert.assertEquals("Expected status is 404", Status.NOT_FOUND.getStatusCode(), resHttp.getStatus());
        Assert.assertNotNull(resEntity);
        Assert.assertTrue("Invalid error message : " + resEntity, resEntity.contains("not exist"));
    }

    /**
     * TDD.
     */
    @Test
    public void testPost_isFlippedInvalidParameter() {
        // Given
        assertFF4J.assertThatFeatureExist(F4);
        // When
        MultivaluedMap<String, String> formData = new MultivaluedMapImpl();
        formData.add(ROLE_NEW, F4);
        ClientResponse resHttp = resourceff4j().path(OPERATION_FLIP).post(ClientResponse.class, formData);
        String resEntity = resHttp.getEntity(String.class);
        // Then
        Assert.assertEquals("Expected status is 400", Status.BAD_REQUEST.getStatusCode(), resHttp.getStatus());
        Assert.assertNotNull(resEntity);
        Assert.assertTrue(resEntity.contains("POST parameter"));
    }

    /**
     * TDD.
     */
    @Test
    public void testPost_isFlippedWithParameters() {
        // Given
        ff4j.getStore().disable(F2);
        assertFF4J.assertThatFeatureExist(F4);
        assertFF4J.assertThatFeatureNotFlipped(F4);
        // When
        MultivaluedMap<String, String> formData = new MultivaluedMapImpl();
        formData.add(POST_PARAMNAME_FEATURE_UID, F4);
        formData.add(POST_PARAMNAME_CUSTOM_PREFIX + "1", "SECOND");
        formData.add(POST_PARAMNAME_CUSTOM_PREFIX + "0", "FIRST");
        formData.add(POST_PARAMNAME_CUSTOM_PREFIX + "2", "T1");
        formData.add(POST_PARAMNAME_CUSTOM_PREFIX + "2", "T2");
        ClientResponse resHttp = resourceff4j().path(OPERATION_FLIP).post(ClientResponse.class, formData);
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
    public void testPost_isFlippedWithCustomStrategy() {
        // Given
        assertFF4J.assertThatFeatureExist(F4);
        assertFF4J.assertThatFeatureNotFlipped(F4);
        // When
        MultivaluedMap<String, String> formData = new MultivaluedMapImpl();
        formData.add(POST_PARAMNAME_FEATURE_UID, F4);
        formData.add(POST_PARAMNAME_CUSTOM_PREFIX + "0", "first");
        String  fs = FeatureJsonMarshaller.renderFlippingStrategy(ff4j.getStore().read(F4).getFlippingStrategy());
        formData.add(POST_PARAMNAME_FLIPSTRATEGY, fs.replace(",\"flippingStrategy\":", ""));
        ClientResponse resHttp = resourceff4j().path(OPERATION_FLIP).post(ClientResponse.class, formData);
        String resEntity = resHttp.getEntity(String.class);
        // Then
        Assert.assertEquals("Expected status is 200", Status.OK.getStatusCode(), resHttp.getStatus());
        Assert.assertNotNull(resEntity);
        Assert.assertTrue(Boolean.valueOf(resEntity));
    }

}
