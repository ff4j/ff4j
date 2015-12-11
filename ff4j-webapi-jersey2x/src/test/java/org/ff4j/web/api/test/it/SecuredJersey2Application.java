package org.ff4j.web.api.test.it;

import org.ff4j.FF4j;
import org.ff4j.utils.Util;
import org.ff4j.web.ApiConfig;
import org.ff4j.web.ApiConfigBuilder;
import org.ff4j.web.api.FF4jApiApplicationJersey2x;

public class SecuredJersey2Application extends FF4jApiApplicationJersey2x {

    private FF4j ff4j = new FF4j();
    
    public SecuredJersey2Application(FF4j ff) {
        this.ff4j = ff;
    }
    
    /** {@inheritDoc} */
    @Override
    public ApiConfig getWebApiConfiguration() {
        ApiConfig secured = new ApiConfigBuilder(ff4j)//
                .withAuthentication() //
                .withAutorization().build();
        
        // Create 2 valid apiKeys
        secured.createApiKey("123", true, false, Util.set("ROLE_USER", "ROLE_ADMIN"));
        secured.createApiKey("456", true, true, Util.set("ROLE_USER", "ROLE_ADMIN"));
        
        secured.createUser("user", "user", true, false, Util.set("ROLE_USER", "ROLE_ADMIN"));
        secured.createUser("admin", "admin", true, true, Util.set("ADMINISTRATOR", "USER"));
        
        return secured;
    }
    
    

}

