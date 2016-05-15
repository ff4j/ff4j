package org.ff4j.web.api.test.it;

import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Response;

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

import static org.ff4j.test.TestsFf4jConstants.*;
import static org.ff4j.web.FF4jWebConstants.*;

/**
 * Integration test for resources {@link FeatureResource}.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureResource2PostTestIT extends AbstractWebResourceTestIT {
   
    /**
     * TDD.
     */
    @Test
    public void testPost_enable() {
        // Given
        assertFF4J.assertThatFeatureExist(F2);
        assertFF4J.assertThatFeatureIsDisabled(F2);
        // When
        WebTarget wResf4 = resourceFeatures().path(F2);
        Response resHttp = wResf4.path(OPERATION_ENABLE).request().post(Entity.text(""));
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 204", Status.NO_CONTENT.getStatusCode(), resHttp.getStatus());
        // Then, Entity Object
        assertFF4J.assertThatFeatureIsEnabled(F2);
    }

    /**
     * TDD.
     */
    @Test
    public void testPost_enableNotFound() {
        // Given
        assertFF4J.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
        // When
        WebTarget wResf4 = resourceFeatures().path(F_DOESNOTEXIST);
        Response resHttp = wResf4.path(OPERATION_ENABLE).request().post(Entity.text(""));
        String resEntity = resHttp.readEntity(String.class);
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
    public void testPost_disable() {
        // Given
        assertFF4J.assertThatFeatureExist(F4);
        assertFF4J.assertThatFeatureIsEnabled(F4);
        // When
        WebTarget wResf4 = resourceFeatures().path(F4);
        Response resHttp = wResf4.path(OPERATION_DISABLE).request().post(Entity.text(""));
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 204", Status.NO_CONTENT.getStatusCode(), resHttp.getStatus());
        // Then, Entity Object
        assertFF4J.assertThatFeatureIsDisabled(F4);
    }

    /**
     * TDD.
     */
    @Test
    public void testPost_disableNotFound() {
        // Given
        assertFF4J.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
        // When
        WebTarget wResf4 = resourceFeatures().path(F_DOESNOTEXIST);
        Response resHttp = wResf4.path(OPERATION_DISABLE).request().post(Entity.text(""));
        String resEntity = resHttp.readEntity(String.class);
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
    public void testPost_grantRole() {
        // Given
        assertFF4J.assertThatFeatureExist(F2);
        assertFF4J.assertThatFeatureHasNotRole(F2, ROLE_NEW);
        // When
        WebTarget wResf4 = resourceFeatures().path(F2);
        Response resHttp = wResf4.path(OPERATION_GRANTROLE).path(ROLE_NEW).request().post(Entity.text(""));
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 204", Status.NO_CONTENT.getStatusCode(), resHttp.getStatus());
        // Then, Entity Object
        assertFF4J.assertThatFeatureHasRole(F2, ROLE_NEW);
    }

    /**
     * TDD.
     */
    @Test
    public void testPost_grantRoleFeatureDoesNotExist() {
        // Given
        assertFF4J.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
        // When
        WebTarget wResf4 = resourceFeatures().path(F_DOESNOTEXIST);
        Response resHttp = wResf4.path(OPERATION_GRANTROLE).path(ROLE_NEW).request().post(Entity.text(""));
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 404", Status.NOT_FOUND.getStatusCode(), resHttp.getStatus());
        // Then, Entity (erro message)
        String resEntity = resHttp.readEntity(String.class);
        Assert.assertNotNull(resEntity);
        Assert.assertTrue("Invalid error message : " + resEntity, resEntity.contains("not exist"));
    }

    /**
     * TDD.
     */
    @Test
    public void testPost_removeRole() {
        // Given
        assertFF4J.assertThatFeatureExist(F2);
        assertFF4J.assertThatFeatureHasRole(F2, ROLE_USER);
        // When
        WebTarget wResf4 = resourceFeatures().path(F2);
        Response resHttp = wResf4.path(OPERATION_REMOVEROLE).path(ROLE_USER).request().post(Entity.text(""));
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 204", Status.NO_CONTENT.getStatusCode(), resHttp.getStatus());
        // Then, Entity Object
        assertFF4J.assertThatFeatureHasNotRole(F2, ROLE_USER);
    }

    /**
     * TDD.
     */
    @Test
    public void testPost_removeRoleFeatureDoesNotExist() {
        // Given
        assertFF4J.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
        // When
        WebTarget wResf4 = resourceFeatures().path(F_DOESNOTEXIST);
        Response resHttp = wResf4.path(OPERATION_REMOVEROLE).path(ROLE_NEW).request().post(Entity.text(""));
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 404", Status.NOT_FOUND.getStatusCode(), resHttp.getStatus());
        // Then, Entity (erro message)
        String resEntity = resHttp.readEntity(String.class);
        Assert.assertNotNull(resEntity);
        Assert.assertTrue("Invalid error message : " + resEntity, resEntity.contains("not exist"));
    }

    /**
     * TDD.
     */
    @Test
    public void testPost_removeRoleInvalidParameter() {
        // Given
        assertFF4J.assertThatFeatureExist(F2);
        Assert.assertFalse(ff4j.getFeatureStore().read(F2).getPermissions().contains(ROLE_READ));
        // When
        WebTarget wResf4 = resourceFeatures().path(F2);
        Response resHttp = wResf4.path(OPERATION_REMOVEROLE).path(ROLE_READ).request().post(Entity.text(""));
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 400", Status.BAD_REQUEST.getStatusCode(), resHttp.getStatus());
        // Then, Entity Object
        String resEntity = resHttp.readEntity(String.class);
        Assert.assertNotNull(resEntity);
        Assert.assertTrue(resEntity.contains("Invalid role"));
    }

    /**
     * TDD.
     */
    @Test
    public void testPost_addToGroup() {
        // Given
        assertFF4J.assertThatFeatureExist(F2);
        assertFF4J.assertThatFeatureNotInGroup(F2, G1);
        // When
        WebTarget wResf4 = resourceFeatures().path(F2);
        Response resHttp = wResf4.path(OPERATION_ADDGROUP).path(G1).request().post(Entity.text(""));
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 204", Status.NO_CONTENT.getStatusCode(), resHttp.getStatus());
        // Then, Entity Object
        assertFF4J.assertThatFeatureIsInGroup(F2, G1);
    }

    /**
     * TDD.
     */
    @Test
    public void testPost_addToGroupFeatureDoesNotExist() {
        // Given
        assertFF4J.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
        // When
        WebTarget wResf4 = resourceFeatures().path(F_DOESNOTEXIST);
        Response resHttp = wResf4.path(OPERATION_ADDGROUP).path(G1).request().post(Entity.text(""));
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 404", Status.NOT_FOUND.getStatusCode(), resHttp.getStatus());
        // Then, Entity (erro message)
        String resEntity = resHttp.readEntity(String.class);
        Assert.assertNotNull(resEntity);
        Assert.assertTrue("Invalid error message : " + resEntity, resEntity.contains("not exist"));
    }

    /**
     * TDD.
     */
    @Test
    public void testPost_removeFromGroup() {
        // Given
        assertFF4J.assertThatFeatureExist(F2);
        ff4j.getFeatureStore().addToGroup(F2, G0);
        assertFF4J.assertThatFeatureIsInGroup(F2, G0);
        // When
        WebTarget wResf4 = resourceFeatures().path(F2);
        Response resHttp = wResf4.path(OPERATION_REMOVEGROUP).path(G0).request().post(Entity.text(""));
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 204", Status.NO_CONTENT.getStatusCode(), resHttp.getStatus());
        // Then, Entity Object
        assertFF4J.assertThatFeatureNotInGroup(F2, G1);
    }

    /**
     * TDD.
     */
    @Test
    public void testPost_removeFromGroupFeatureDoesNotExist() {
        // Given
        assertFF4J.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
        // When
        WebTarget wResf4 = resourceFeatures().path(F_DOESNOTEXIST);
        Response resHttp = wResf4.path(OPERATION_REMOVEGROUP).path(G1).request().post(Entity.text(""));
        
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 404", Status.NOT_FOUND.getStatusCode(), resHttp.getStatus());
        // Then, Entity (erro message)
        String resEntity = resHttp.readEntity(String.class);
        Assert.assertNotNull(resEntity);
        Assert.assertTrue("Invalid error message : " + resEntity, resEntity.contains("not exist"));
    }

    /**
     * TDD.
     */
    @Test
    public void testPost_removeFromGroupInvalidParameter() {
        // Given
        assertFF4J.assertThatFeatureExist(F2);
        assertFF4J.assertThatFeatureNotInGroup(F2, G1);
        // When
        WebTarget wResf4 = resourceFeatures().path(F2);
        Response resHttp = wResf4.path(OPERATION_REMOVEGROUP).path("GRPXX").request().post(Entity.text(""));
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 400", Status.BAD_REQUEST.getStatusCode(), resHttp.getStatus());
        // Then, Entity Object
        String resEntity = resHttp.readEntity(String.class);
        Assert.assertNotNull(resEntity);
        Assert.assertTrue(resEntity.contains("Invalid groupName"));
    }

}
