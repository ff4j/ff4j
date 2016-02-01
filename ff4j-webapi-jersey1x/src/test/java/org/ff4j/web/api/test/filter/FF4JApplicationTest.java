package org.ff4j.web.api.test.filter;

/*
 * #%L
 * ff4j-webapi-jersey1x
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
import org.ff4j.web.api.FF4JApiApplication;
import org.ff4j.web.api.security.FF4jRolesResourceFilterFactory;
import org.junit.Assert;
import org.junit.Test;
import org.mockito.Mockito;

import com.sun.jersey.api.model.AbstractMethod;
import com.sun.jersey.api.model.AbstractResource;

public class FF4JApplicationTest {

    @Test
    public void testApplicationInitialization() {
        new FF4JApiApplication() {
            protected ApiConfig getApiConfig() {
                ApiConfig ac = new ApiConfig();
                ac.setAutorize(true);
                return ac;
            }
        };

        new FF4JApiApplication() {
            protected ApiConfig getApiConfig() {
                ApiConfig ac = new ApiConfig();
                ac.setAuthenticate(true);
                ac.setLog(true);
                return ac;
            }
        };

        Assert.assertNotNull(new FF4JApiApplication() {
            protected ApiConfig getApiConfig() {
                ApiConfig ac = new ApiConfig();
                ac.setAuthenticate(false);
                ac.setLog(false);
                return ac;
            }
        });
    }
    
    @Test
    public void testInitiateRoleFactory() {
        FF4jRolesResourceFilterFactory fff = new FF4jRolesResourceFilterFactory();
        AbstractMethod am = Mockito.mock(AbstractMethod.class);
        Mockito.when(am.getResource()).thenReturn(Mockito.mock(AbstractResource.class));
        fff.create(am);
        Assert.assertNotNull(fff);
    }
}
