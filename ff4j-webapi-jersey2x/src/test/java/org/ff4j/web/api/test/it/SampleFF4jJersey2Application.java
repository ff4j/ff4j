package org.ff4j.web.api.test.it;

import org.ff4j.FF4j;
import org.ff4j.web.FF4jApiConfig;
import org.ff4j.web.api.AbstractJersey2ApiApplication;

public class SampleFF4jJersey2Application extends AbstractJersey2ApiApplication {

    private FF4j ff4j = new FF4j("ff4j.xml");
    
    @Override
    protected FF4jApiConfig getWebApiConfiguration() {
         FF4jApiConfig fac = new FF4jApiConfig(ff4j);
         fac.documentation(true);
         fac.setPort(3388);
         fac.setHost("localhost");
         fac.setWebContext("webapi");
         return fac;
    }

}
