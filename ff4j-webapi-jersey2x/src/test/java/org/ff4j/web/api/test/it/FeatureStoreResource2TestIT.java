package org.ff4j.web.api.test.it;

import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.ff4j.store.InMemoryFeatureStore;
import org.junit.Assert;
import org.junit.Test;

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

public class FeatureStoreResource2TestIT extends AbstractWebResourceTestIT {

    /**
     * TDD.
     */
    @Test
    public void testGet() {
        // Given
        Assert.assertTrue(ff4j.getFeatureStore() instanceof InMemoryFeatureStore);
        // When
        WebTarget wResFeatures = resourceStore();
        Response httpResponse = wResFeatures.request().get();
        String resEntity = httpResponse.readEntity(String.class);
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 200", Status.OK.getStatusCode(), httpResponse.getStatus());
        Assert.assertNotNull(resEntity);
        // Then, Entity Object
        Assert.assertTrue(resEntity.contains(InMemoryFeatureStore.class.getCanonicalName()));
    }

}
