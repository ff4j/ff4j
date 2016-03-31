package org.ff4j.web.resources.it;

import static org.ff4j.utils.json.FeatureJsonParser.parseFeatureArray;
import java.util.HashSet;
import java.util.Set;

import javax.ws.rs.core.Response.Status;

import org.junit.Assert;

import org.ff4j.core.Feature;
import org.junit.Test;

import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;

import static org.ff4j.test.TestsFf4jConstants.*;

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

/**
 * Unit testing of resource Groups/group
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class GroupResourceTestIT extends AbstractWebResourceTestIT {

    /**
     * TDD
     */
    @Test
    public void testGet_readGroup() {
        // Given
        assertFF4J.assertThatGroupExist(G1);
        assertFF4J.assertThatGroupHasSize(2, G1);
        assertFF4J.assertThatFeatureIsInGroup(F3, G1);
        assertFF4J.assertThatFeatureIsInGroup(F4, G1);
        // When
        WebResource wrsc = resourceGroups().path(G1);
        ClientResponse resHttp = wrsc.get(ClientResponse.class);
        String resEntity = resHttp.getEntity(String.class);
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 200", Status.OK.getStatusCode(), resHttp.getStatus());
        // Then, Entity Object
        Assert.assertNotNull(resEntity);
        Feature[] f = parseFeatureArray(resEntity);
        Set<String> features = new HashSet<String>();
        for (Feature feature : f) {
            features.add(feature.getUid());
        }
        Assert.assertEquals(2, features.size());
        Assert.assertTrue(features.contains(F3));
        Assert.assertTrue(features.contains(F4));
    }
    
    @Test
    public void getGroupNotFound() {
    }
    
    @Test
    public void testPost_EnableGroup() {}

    @Test
    public void testPost_EnableGroupNotFound() {}

    @Test
    public void testPost_DisableGroup() {
    }
    
    @Test
    public void testPost_DisableGroupNotFound() {
    }
}
