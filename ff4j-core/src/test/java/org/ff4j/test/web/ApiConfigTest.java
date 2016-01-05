package org.ff4j.test.web;

import java.util.HashMap;
import java.util.Set;

import org.ff4j.FF4j;
import org.ff4j.utils.Util;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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


import org.ff4j.web.ApiConfig;
import org.ff4j.web.ApiConfigBuilder;
import org.junit.Assert;
import org.junit.Test;

public class ApiConfigTest {
    
    @Test
    public void testApiConfig() {
        ApiConfig apc1 = new ApiConfig();
        apc1.setVersion("1");apc1.getVersion();
        apc1.isAuthenticate();apc1.isAutorize();apc1.isDocumentation();apc1.isLog();
        apc1.setFF4j(new FF4j());apc1.getFF4j();
        apc1.setApiKeys(Util.set("1", "2"));apc1.getApiKeys();
        apc1.setAuthenticate(false);
        apc1.setAutorize(false);
        apc1.setHost("localhost");apc1.getHost();
        apc1.setPort(8080);apc1.getPort();
        apc1.setPermissions(new HashMap<String, Set<String>>());
        apc1.getPermissions();
        apc1.setLog(false);
        apc1.setUsers(new HashMap<String, String>());
        apc1.getUsers();
        apc1.getWebContext();
        apc1.getContextPath();
        apc1.createApiKey("key", true, true, Util.set("USER", "ADMIN"));
        apc1.createUser("john", "tiger", true, true, Util.set("USER", "ADMIN"));
        apc1.setFF4j(new FF4j());
        new ApiConfigBuilder(apc1);
    }
    
    @Test
    public void tesApiConfigBuilder() {
        ApiConfigBuilder b = new ApiConfigBuilder();
        b.addApiKey("123").addUser("", "")
        .authenticate(false)
        .documentation(false).host("localhost").port(12)
        .webContext("/ok")
        .withAuthentication().withAutorization()
        .withDocumentation().withoutAuthentication()
        .withoutAutorization().withoutDocumentation();
        ApiConfig conf = b.build();
        Assert.assertNotNull(conf);
    
        new ApiConfigBuilder(new FF4j());
    }

}
