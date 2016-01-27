package org.ff4j.web.api.test;

import org.ff4j.web.api.FF4jTracingContextFilter;
import org.junit.Assert;
import org.junit.Test;

public class SecurityContextFilterTest {
    
    @Test
    public void initSecurityFilter() {
        FF4jTracingContextFilter ctx = new FF4jTracingContextFilter();
        Assert.assertNotNull(ctx.getRequestFilter());
        Assert.assertNotNull(ctx.getResponseFilter());
    }

}
