package org.ff4j.web.api.test;

import org.ff4j.FF4j;
import org.ff4j.web.ApiConfig;
import org.ff4j.web.api.FF4jApiApplicationJersey2x;

public class SampleFF4jJersey2Application extends FF4jApiApplicationJersey2x {

    private FF4j ff4j = new FF4j();
    
    @Override
    protected ApiConfig getWebApiConfiguration() {
         ApiConfig fac = new ApiConfig(ff4j);
         fac.setDocumentation(true);
         fac.setPort(3388);
         fac.setHost("localhost");
         fac.setWebContext("webapi");
         return fac;
    }

}