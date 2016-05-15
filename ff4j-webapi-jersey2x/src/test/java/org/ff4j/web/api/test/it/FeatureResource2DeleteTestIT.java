package org.ff4j.web.api.test.it;

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

/**
 * Integration test for resources {@link FeatureResource}.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureResource2DeleteTestIT extends AbstractWebResourceTestIT {

    /**
     * TDD.
     */
    @Test
    public void testGet_delete() {
        // Given
        assertFF4J.assertThatFeatureExist(F1);
        // When
        WebTarget wResf4 = resourceFeatures().path(F1);
        Response resHttp = wResf4.request().delete();
        
        // Then, HTTP Response
        Assert.assertEquals("Expected status is 204", Status.NO_CONTENT.getStatusCode(), resHttp.getStatus());
        // Then, Store state
        assertFF4J.assertThatFeatureDoesNotExist(F1);
    }

    /**
     * TDD.
     */
    @Test
    public void testGet_deleteNotFound() {
        // Given
        assertFF4J.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
        // When
        WebTarget wResf4 = resourceFeatures().path(F_DOESNOTEXIST);
        Response resHttp = wResf4.request().delete();
        // Then, HTTP Response
        String resEntity = resHttp.readEntity(String.class);
        Assert.assertEquals("Expected status is 404", Status.NOT_FOUND.getStatusCode(), resHttp.getStatus());
        // Then, Entity (erro message)
        Assert.assertNotNull(resEntity);
        Assert.assertTrue("Invalid error message : " + resEntity, resEntity.contains("not exist"));
    }


}
