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

import static org.ff4j.utils.json.FeatureJsonParser.parseFeatureArray;

import javax.ws.rs.core.Response.Status;

import org.ff4j.core.Feature;
import org.junit.Assert;
import org.junit.Test;

import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;

/**
 * Integration test for "/features" (readAll) resource.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeaturesResourceTestIT extends AbstractWebResourceTestIT {

    /**
     * TDD.
     */
    @Test
    public void testGet() {
        // Given
        assertFF4J.assertThatStoreHasSize(EXPECTED_FEATURES_NUMBERS);
        // When
        WebResource wResFeatures = resourceFeatures();
        ClientResponse httpResponse = wResFeatures.get(ClientResponse.class);
        String resEntity = httpResponse.getEntity(String.class);
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 200", Status.OK.getStatusCode(), httpResponse.getStatus());
        Assert.assertNotNull(resEntity);
        // Then, Entity Object
        Feature[] fArray = parseFeatureArray(resEntity);
        Assert.assertEquals(EXPECTED_FEATURES_NUMBERS, fArray.length);
    }

}
