package org.ff4j.web.api.test.it;

import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;

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
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.ff4j.core.Feature;
import org.ff4j.web.api.resources.domain.FeatureApiBean;
import org.junit.Assert;
import org.junit.Test;

import static org.ff4j.test.TestsFf4jConstants.*;

/**
 * Externalisation of PUT REQUEST.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureResource2PutUpdateGroup1TestIT extends AbstractWebResourceTestIT {

    /**
     * TDD, update by adding in the authorization
     */
    @Test
    public void testPut_upsertUpdateAddGroup() throws Exception {
        // Given
        assertFF4J.assertThatFeatureExist(F1);
        assertFF4J.assertThatFeatureNotInGroup(F1, "g2");
        // When
        Feature f1 = ff4j.getFeature(F1);
        f1.setGroup("g2");
        
        WebTarget webResFeat = resourceFeatures().path(F1);
        Response res = webResFeat//
                .request(MediaType.APPLICATION_JSON)//
                .put(Entity.entity(new FeatureApiBean(f1), MediaType.APPLICATION_JSON), Response.class);
                
        // Then HTTPResponse
        Assert.assertEquals(Status.NO_CONTENT.getStatusCode(), res.getStatus());
        // Then Object Entity : null
        // Then
        assertFF4J.assertThatFeatureIsInGroup(F1, "g2");
    }


}
