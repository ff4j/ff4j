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

import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response.Status;

import org.junit.Assert;
import org.ff4j.core.Feature;
import org.ff4j.web.api.resources.domain.FeatureApiBean;
import org.junit.Test;

import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;

import static org.ff4j.test.TestsFf4jConstants.*;
import static org.ff4j.web.FF4jWebConstants.*;

/**
 * Externalisation of PUT REQUEST.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureResourcePutCreateTestIT extends AbstractWebResourceTestIT {

    /**
     * TDD.
     */
    @Test
    public void testPutUpsertIfNotExistCreateIt() {
        // Given
        assertFF4J.assertThatFeatureDoesNotExist(FEATURE_X);
        // When
        Feature f = new Feature(FEATURE_X);
        WebResource webResFeat = resourceFeatures().path(FEATURE_X);
        ClientResponse res = webResFeat//
                .type(MediaType.APPLICATION_JSON)//
                .put(ClientResponse.class, new FeatureApiBean(f));
        // Then HTTPResponse
        Assert.assertEquals(Status.CREATED.getStatusCode(), res.getStatus());
        Assert.assertNotNull(res.getHeaders().getFirst(LOCATION));
        // Then, testing target store
        assertFF4J.assertThatFeatureExist(FEATURE_X);
        res.close();
    }


}
