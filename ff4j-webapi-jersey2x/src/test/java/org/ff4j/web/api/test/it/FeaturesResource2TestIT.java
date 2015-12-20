package org.ff4j.web.api.test.it;

/*
 * #%L
 * ff4j-webapi-jersey2x
 * %%
 * Copyright (C) 2013 - 2015 FF4J
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


import java.util.List;

import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.junit.Assert;
import org.junit.Test;

/**
 * Integration test for "/features" (readAll) resource.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeaturesResource2TestIT extends AbstractWebResourceTestIT {

    /**
     * TDD.
     */
    @Test
    public void testGet() {
        // Given
        // When
        WebTarget wResFeatures = resourceFeatures();
        Response httpResponse = wResFeatures.request().get();
        
        List<?> fList = httpResponse.readEntity(List.class);
        
        // Then, HTTPResponse
        Assert.assertEquals("Expected status is 200", Status.OK.getStatusCode(), httpResponse.getStatus());
        Assert.assertNotNull(fList);
        // Then, Entity Object
        Assert.assertFalse(fList.isEmpty());
    }

}
