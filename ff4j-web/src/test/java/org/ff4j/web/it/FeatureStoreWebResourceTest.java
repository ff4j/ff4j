package org.ff4j.web.it;

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

import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import junit.framework.Assert;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.test.AssertFf4j;
import org.ff4j.test.TestsFf4jConstants;
import org.ff4j.utils.FeatureJsonMarshaller;
import org.ff4j.web.resources.FeaturesResource;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * Test {@link FeaturesResource} as simple javacode.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("classpath:test-WebApi-context.xml")
@Ignore
@Deprecated
public class FeatureStoreWebResourceTest implements TestsFf4jConstants {

    @Autowired
    private FeaturesResource api;

    /** To test in memory store, after remote operations. */
    @Autowired
    private FF4j ff4j;

    /** Assert for this ff4j instance. */
    private AssertFf4j assertFF4J;

    /** Testing feature. */
    private Feature f4;

    /** {@inheritDoc} */
    @Before
    public void setUp() throws Exception {
        assertFF4J = new AssertFf4j(ff4j);
        f4 = ff4j.getFeature(F4);
    }


    /**
     * TDD, update by adding in the authorization
     */
    @Test
    public void upsertUpdateAddAuthorization() {
        // Given
        assertFF4J.assertThatFeatureExist(FEATURE_NEW);
        assertFF4J.assertThatFeatureHasNotRole(FEATURE_NEW, ROLE_NEW);
        // When
        // Feature fNew = FeatureJsonMarshaller.unMarshallFeature(String.valueOf(api.read(FEATURE_NEW).getEntity()));
        // fNew.getAuthorizations().add(ROLE_NEW);
        // api.upsert(fNew);
        // Then
        assertFF4J.assertThatFeatureHasRole(FEATURE_NEW, ROLE_NEW);
    }

    /**
     * TDD.
     */
    @Test
    public void upsertUpdateRemoveAuthorization() {
        // Given
        assertFF4J.assertThatFeatureExist(FEATURE_NEW);
        assertFF4J.assertThatFeatureHasRole(FEATURE_NEW, ROLE_NEW);
        // When
        // Feature fNew = FeatureJsonMarshaller.unMarshallFeature(String.valueOf(api.read(FEATURE_NEW).getEntity()));
        // fNew.getAuthorizations().remove(ROLE_NEW);
        // api.upsert(fNew);
        // Then
        assertFF4J.assertThatFeatureHasNotRole(FEATURE_NEW, ROLE_NEW);
    }
    
    /**
     * TDD.
     */
    @Test
    public void upsertUpdateAddToGroup() {
        // Given
        assertFF4J.assertThatFeatureExist(FEATURE_NEW);
        assertFF4J.assertThatFeatureNotInGroup(FEATURE_NEW, G0);
        // When
        // Feature fNew = FeatureJsonMarshaller.unMarshallFeature(String.valueOf(api.read(FEATURE_NEW).getEntity()));
        // fNew.setGroup(G0);
        // api.upsert(fNew);
        // Then
        assertFF4J.assertThatFeatureIsInGroup(FEATURE_NEW, G0);
    }

    @Test
    public void upsertUpdateRemoveFromGroup() {
        // Given
        assertFF4J.assertThatFeatureExist(FEATURE_NEW);
        assertFF4J.assertThatFeatureIsInGroup(FEATURE_NEW, G0);
        // When
        // Feature fNew = FeatureJsonMarshaller.unMarshallFeature(String.valueOf(api.read(FEATURE_NEW).getEntity()));
        // fNew.setGroup("");
        // api.upsert(fNew);
        // Then
        assertFF4J.assertThatFeatureNotInGroup(FEATURE_NEW, G0);
        // Restore previous state
        ff4j.getStore().delete(FEATURE_NEW);
    }

    /**
     * TDD, disable a feature
     */
    @Test
    public void upsertUpdateEnable() {
        // Given
        assertFF4J.assertThatFeatureExist(F1);
        assertFF4J.assertThatFeatureIsEnabled(F1);
        // When
        // Feature f1 = FeatureJsonMarshaller.unMarshallFeature(String.valueOf(api.read(F1).getEntity()));
        // f1.setEnable(false);
        // api.upsert(f1);
        // Then
        assertFF4J.assertThatFeatureIsDisabled(F1);
    }

    /**
     * TDD, read all feature as Array from REST Resource
     */
    @Test
    public void testReadAll() {
        // Given
        assertFF4J.assertThatStoreHasSize(5);
        // When
        Response res = api.readAll();
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 200", Status.OK.getStatusCode(), res.getStatus());
        Assert.assertNotNull(res.getEntity());
        Assert.assertTrue("Invalid response " + res.getEntity(), res.getEntity() instanceof String);
        // Then, Entity Object
        Feature[] fArray = FeatureJsonMarshaller.unMarshallFeatureArray(String.valueOf(res.getEntity()));
        Assert.assertEquals(5, fArray.length);
    }

    /**
     * TDD, delete f1
     */
    @Test
    public void testDeleteOK() {
        // Given
        assertFF4J.assertThatStoreHasSize(5);
        assertFF4J.assertThatFeatureExist(F1);
        Feature f1 = ff4j.getStore().read(F1);
        // When
        // Response res = api.delete(F1);
        Response res = null;

        // Then, HTTP Response
        // Assert.assertEquals("Expected status is 204", Status.NO_CONTENT.getStatusCode(), res.getStatus());
        Assert.assertNull(res.getEntity());
        // Then, Store state
        assertFF4J.assertThatStoreHasSize(4);
        assertFF4J.assertThatFeatureDoesNotExist(F1);
        // Restore previous state
        ff4j.getStore().create(f1);
    }

    /**
     * TDD, delete not found will get 404
     */
    @Test
    public void testDeleteNotFound() {
        // Given
        assertFF4J.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
        // When
        // Response res = api.delete(F_DOESNOTEXIST);
        Response res = null;

        // Then, HTTP Response
        Assert.assertEquals("Expected status is 404", Status.NOT_FOUND.getStatusCode(), res.getStatus());
        Assert.assertNotNull(res.getEntity());
        Assert.assertTrue(String.valueOf(res.getEntity()).contains("does not exist"));
    }

    /**
     * TDD, delete with invalid parameter, get 400
     */
    @Test
    public void testDeleteInvalidParameter() {
        // Given
        // When
        // Response res = api.delete("");
        Response res = null;
        // Then HTTP Response
        Assert.assertEquals("Expected status is 400", Status.BAD_REQUEST.getStatusCode(), res.getStatus());
        // Then, Entity (erro message)
        Assert.assertNotNull(res.getEntity());
        Assert.assertTrue(String.valueOf(res.getEntity()).contains("not null nor empty"));
    }

}