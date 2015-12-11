package org.ff4j.web.api.test.it;

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
