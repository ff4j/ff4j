package org.ff4j.web.resources.it;

import javax.ws.rs.core.Response.Status;

import org.ff4j.property.store.InMemoryPropertyStore;
import org.junit.Assert;
import org.junit.Test;

import com.sun.jersey.api.client.ClientResponse;

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

public class PropertyStoreResourceTestIT extends AbstractWebResourceTestIT {

    /**
     * TDD.
     */
    @Test
    public void testGet() {
        // Given
        Assert.assertTrue(ff4j.getPropertiesStore() instanceof InMemoryPropertyStore);
        // When
        ClientResponse resHttp = rscPropertyStore().get(ClientResponse.class);
        String resEntity = resHttp.getEntity(String.class);
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 200", Status.OK.getStatusCode(), resHttp.getStatus());
        Assert.assertNotNull(resEntity);
        // Then, Entity Object
        Assert.assertTrue(resEntity.contains(InMemoryPropertyStore.class.getCanonicalName()));
    }

}
