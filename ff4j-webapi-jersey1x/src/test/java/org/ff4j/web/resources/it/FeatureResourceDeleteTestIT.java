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

import org.junit.Assert;
import org.ff4j.web.api.resources.FeatureResource;
import org.junit.Test;

import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;

import static org.ff4j.test.TestsFf4jConstants.*;

/**
 * Integration test for resources {@link FeatureResource}.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureResourceDeleteTestIT extends AbstractWebResourceTestIT {

    /**
     * TDD.
     */
    @Test
    public void testGetDelete() {
        // Given
        assertFF4J.assertThatFeatureExist(F1);
        // When
        WebResource wResf4 = resourceFeatures().path(F1);
        ClientResponse resHttp = wResf4.delete(ClientResponse.class);
        // Then, HTTP Response
        Assert.assertEquals("Expected status is 204", Status.NO_CONTENT.getStatusCode(), resHttp.getStatus());
        // Then, Store state
        assertFF4J.assertThatFeatureDoesNotExist(F1);
    }

    /**
     * TDD.
     */
    @Test
    public void testGetDeleteNotFound() {
        // Given
        assertFF4J.assertThatFeatureDoesNotExist(F_DOESNOTEXIST);
        // When
        WebResource wResf4 = resourceFeatures().path(F_DOESNOTEXIST);
        ClientResponse resHttp = wResf4.delete(ClientResponse.class);
        String resEntity = resHttp.getEntity(String.class);
        // Then, HTTP Response
        Assert.assertEquals("Expected status is 404", Status.NOT_FOUND.getStatusCode(), resHttp.getStatus());
        // Then, Entity (erro message)
        Assert.assertNotNull(resEntity);
        Assert.assertTrue("Invalid error message : " + resEntity, resEntity.contains("not exist"));
    }


}
