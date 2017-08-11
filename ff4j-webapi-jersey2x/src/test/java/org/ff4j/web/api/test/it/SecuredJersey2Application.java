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

