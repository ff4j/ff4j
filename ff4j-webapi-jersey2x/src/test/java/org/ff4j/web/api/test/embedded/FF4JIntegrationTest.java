package org.ff4j.web.api.test.embedded;

import java.io.IOException;

import javax.ws.rs.core.MediaType;

import org.junit.Test;

public class FF4JIntegrationTest extends AbstractEmbeddedGrizzlyIntegrationTest {
    
    @Test
    public void testFF4j() throws IOException {
        target().path("/ff4j").request(MediaType.APPLICATION_JSON).get();
    }

}
