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

import javax.ws.rs.core.Response.Status;

import org.ff4j.web.api.resources.FeatureResource;
import org.junit.Assert;
import org.junit.Test;

import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;

import static org.ff4j.test.TestsFf4jConstants.*;
import static org.ff4j.web.FF4jWebConstants.*;

/**
 * Integration test for resources {@link FeatureResource}.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureResourcePostTestIT extends AbstractWebResourceTestIT {

    /**
     * TDD.
     */
    @Test
    public void testPostEnable() {
        // Given
        assertFF4J.assertThatFeatureExist(F2);
        ff4j.getFeatureStore().disable(F2);
        assertFF4J.assertThatFeatureIsDisabled(F2);
        // When
        WebResource wResf4 = resourceFeatures().path(F2);
        ClientResponse resHttp = wResf4.path(OPERATION_ENABLE).post(ClientResponse.class);
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 204", Status.NO_CONTENT.getStatusCode(), resHttp.getStatus());
        // Then, Entity Object
        assertFF4J.assertThatFeatureIsEnabled(F2);
    }

    /**
     * TDD.
     */
    @Test
    public void testPostEnableNotFound() {
        // Given
        assertFF4J.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
        // When
        WebResource wResf4 = resourceFeatures().path(F_DOESNOTEXIST);
        ClientResponse resHttp = wResf4.path(OPERATION_ENABLE).post(ClientResponse.class);
        String resEntity = resHttp.getEntity(String.class);
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 404", Status.NOT_FOUND.getStatusCode(), resHttp.getStatus());
        // Then, Entity Object
        Assert.assertNotNull(resEntity);
        Assert.assertTrue("Invalid error message : " + resEntity, resEntity.contains("not exist"));
    }

    /**
     * TDD.
     */
    @Test
    public void testPostDisable() {
        // Given
        assertFF4J.assertThatFeatureExist(AWESOME);
        assertFF4J.assertThatFeatureIsEnabled(AWESOME);
        // When
        WebResource wResf4 = resourceFeatures().path(AWESOME);
        ClientResponse resHttp = wResf4.path(OPERATION_DISABLE).post(ClientResponse.class);
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 204", Status.NO_CONTENT.getStatusCode(), resHttp.getStatus());
        // Then, Entity Object
        assertFF4J.assertThatFeatureIsDisabled(AWESOME);
    }

    /**
     * TDD.
     */
    @Test
    public void testPostDisableNotFound() {
        // Given
        assertFF4J.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
        // When
        WebResource wResf4 = resourceFeatures().path(F_DOESNOTEXIST);
        ClientResponse resHttp = wResf4.path(OPERATION_DISABLE).post(ClientResponse.class);
        String resEntity = resHttp.getEntity(String.class);
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 404", Status.NOT_FOUND.getStatusCode(), resHttp.getStatus());
        // Then, Entity Object
        Assert.assertNotNull(resEntity);
        Assert.assertTrue("Invalid error message : " + resEntity, resEntity.contains("not exist"));
    }

    /**
     * TDD.
     */
    @Test
    public void testPostGrantRole() {
        // Given
        assertFF4J.assertThatFeatureExist(F2);
        assertFF4J.assertThatFeatureHasNotRole(F2, ROLE_NEW);
        // When
        WebResource wResf4 = resourceFeatures().path(F2);
        ClientResponse resHttp = wResf4.path(OPERATION_GRANTROLE).path(ROLE_NEW).post(ClientResponse.class);
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 204", Status.NO_CONTENT.getStatusCode(), resHttp.getStatus());
        // Then, Entity Object
        assertFF4J.assertThatFeatureHasRole(F2, ROLE_NEW);
    }

    /**
     * TDD.
     */
    @Test
    public void testPostGrantRoleFeatureDoesNotExist() {
        // Given
        assertFF4J.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
        // When
        WebResource wResf4 = resourceFeatures().path(F_DOESNOTEXIST);
        ClientResponse resHttp = wResf4.path(OPERATION_GRANTROLE).path(ROLE_NEW).post(ClientResponse.class);
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 404", Status.NOT_FOUND.getStatusCode(), resHttp.getStatus());
        // Then, Entity (erro message)
        String resEntity = resHttp.getEntity(String.class);
        Assert.assertNotNull(resEntity);
        Assert.assertTrue("Invalid error message : " + resEntity, resEntity.contains("not exist"));
    }

    /**
     * TDD.
     */
    @Test
    public void testPostRemoveRole() {
        // Given
        assertFF4J.assertThatFeatureExist(F2);
        assertFF4J.assertThatFeatureHasRole(F2, ROLE_USER);
        // When
        WebResource wResf4 = resourceFeatures().path(F2);
        ClientResponse resHttp = wResf4.path(OPERATION_REMOVEROLE).path(ROLE_USER).post(ClientResponse.class);
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 204", Status.NO_CONTENT.getStatusCode(), resHttp.getStatus());
        // Then, Entity Object
        assertFF4J.assertThatFeatureHasNotRole(F2, ROLE_USER);
    }

    /**
     * TDD.
     */
    @Test
    public void testPostRemoveRoleFeatureDoesNotExist() {
        // Given
        assertFF4J.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
        // When
        WebResource wResf4 = resourceFeatures().path(F_DOESNOTEXIST);
        ClientResponse resHttp = wResf4.path(OPERATION_REMOVEROLE).path(ROLE_NEW).post(ClientResponse.class);
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 404", Status.NOT_FOUND.getStatusCode(), resHttp.getStatus());
        // Then, Entity (erro message)
        String resEntity = resHttp.getEntity(String.class);
        Assert.assertNotNull(resEntity);
        Assert.assertTrue("Invalid error message : " + resEntity, resEntity.contains("not exist"));
    }

    /**
     * TDD.
     */
    @Test
    public void testPostRemoveRoleInvalidParameter() {
        // Given
        assertFF4J.assertThatFeatureExist(F2);
        Assert.assertFalse(ff4j.getFeatureStore().read(F2).getPermissions().contains(ROLE_READ));
        // When
        WebResource wResf4 = resourceFeatures().path(F2);
        ClientResponse resHttp = wResf4.path(OPERATION_REMOVEROLE).path(ROLE_READ).post(ClientResponse.class);
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 400", Status.BAD_REQUEST.getStatusCode(), resHttp.getStatus());
        // Then, Entity Object
        String resEntity = resHttp.getEntity(String.class);
        Assert.assertNotNull(resEntity);
        Assert.assertTrue(resEntity.contains("Invalid role"));
    }

    /**
     * TDD.
     */
    @Test
    public void testPostAddToGroup() {
        // Given
        assertFF4J.assertThatFeatureExist(F2);
        assertFF4J.assertThatFeatureNotInGroup(F2, G1);
        // When
        WebResource wResf4 = resourceFeatures().path(F2);
        ClientResponse resHttp = wResf4.path(OPERATION_ADDGROUP).path(G1).post(ClientResponse.class);
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 204", Status.NO_CONTENT.getStatusCode(), resHttp.getStatus());
        // Then, Entity Object
        assertFF4J.assertThatFeatureIsInGroup(F2, G1);
    }

    /**
     * TDD.
     */
    @Test
    public void testPostAddToGroupFeatureDoesNotExist() {
        // Given
        assertFF4J.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
        // When
        WebResource wResf4 = resourceFeatures().path(F_DOESNOTEXIST);
        ClientResponse resHttp = wResf4.path(OPERATION_ADDGROUP).path(G1).post(ClientResponse.class);
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 404", Status.NOT_FOUND.getStatusCode(), resHttp.getStatus());
        // Then, Entity (erro message)
        String resEntity = resHttp.getEntity(String.class);
        Assert.assertNotNull(resEntity);
        Assert.assertTrue("Invalid error message : " + resEntity, resEntity.contains("not exist"));
    }

    /**
     * TDD.
     */
    @Test
    public void testPostRemoveFromGroup() {
        // Given
        assertFF4J.assertThatFeatureExist(F2);
        ff4j.getFeatureStore().addToGroup(F2, G0);
        assertFF4J.assertThatFeatureIsInGroup(F2, G0);
        // When
        WebResource wResf4 = resourceFeatures().path(F2);
        ClientResponse resHttp = wResf4.path(OPERATION_REMOVEGROUP).path(G0).post(ClientResponse.class);
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 204", Status.NO_CONTENT.getStatusCode(), resHttp.getStatus());
        // Then, Entity Object
        assertFF4J.assertThatFeatureNotInGroup(F2, G1);
    }

    /**
     * TDD.
     */
    @Test
    public void testPostRemoveFromGroupFeatureDoesNotExist() {
        // Given
        assertFF4J.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
        // When
        WebResource wResf4 = resourceFeatures().path(F_DOESNOTEXIST);
        ClientResponse resHttp = wResf4.path(OPERATION_REMOVEGROUP).path(G1).post(ClientResponse.class);
        
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 404", Status.NOT_FOUND.getStatusCode(), resHttp.getStatus());
        // Then, Entity (erro message)
        String resEntity = resHttp.getEntity(String.class);
        Assert.assertNotNull(resEntity);
        Assert.assertTrue("Invalid error message : " + resEntity, resEntity.contains("not exist"));
    }

    /**
     * TDD.
     */
    @Test
    public void testPostRemoveFromGroupInvalidParameter() {
        // Given
        assertFF4J.assertThatFeatureExist(F2);
        assertFF4J.assertThatFeatureNotInGroup(F2, G1);
        // When
        WebResource wResf4 = resourceFeatures().path(F2);
        ClientResponse resHttp = wResf4.path(OPERATION_REMOVEGROUP).path("GRPXX").post(ClientResponse.class);
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 400", Status.BAD_REQUEST.getStatusCode(), resHttp.getStatus());
        // Then, Entity Object
        String resEntity = resHttp.getEntity(String.class);
        Assert.assertNotNull(resEntity);
        Assert.assertTrue(resEntity.contains("Invalid groupName"));
    }

}
