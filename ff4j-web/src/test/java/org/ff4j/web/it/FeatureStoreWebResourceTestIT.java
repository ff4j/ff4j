package org.ff4j.web.it;

import java.io.IOException;

import javax.ws.rs.Path;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response.Status;
import javax.ws.rs.ext.Provider;

import junit.framework.Assert;

import org.apache.http.client.ClientProtocolException;
import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.test.AssertFf4j;
import org.ff4j.test.TestConstantsFF4j;
import org.ff4j.utils.FeatureJsonMarshaller;
import org.ff4j.web.resources.FF4jResource;
import org.ff4j.web.resources.FeatureResource;
import org.ff4j.web.resources.FeaturesResource;
import org.ff4j.web.resources.RuntimeExceptionMapper;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.api.core.ClassNamesResourceConfig;
import com.sun.jersey.spi.container.servlet.WebComponent;
import com.sun.jersey.spi.inject.SingletonTypeInjectableProvider;
import com.sun.jersey.test.framework.JerseyTest;
import com.sun.jersey.test.framework.WebAppDescriptor;
import com.sun.jersey.test.framework.spi.container.TestContainerFactory;
import com.sun.jersey.test.framework.spi.container.grizzly2.web.GrizzlyWebTestContainerFactory;

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

/**
 * Integration Test within GRIZZLY.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreWebResourceTestIT extends JerseyTest implements TestConstantsFF4j {
    
    /** Relative resource. */
    private static final String APIPATH = FF4jResource.class.getAnnotation(Path.class).value();

    private static final FF4j ff4j = new FF4j("test-WebApi-ff4j.xml");

    /** Assert for this ff4j instance. */
    private AssertFf4j assertFF4J;

    /** {@inheritDoc} */
    @Override
    @Before
    public void setUp() throws Exception {
        assertFF4J = new AssertFf4j(ff4j);
    }

    /**
     * Provider.
     * 
     * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
     */
    @Provider
    public static class FF4jProvider extends SingletonTypeInjectableProvider<Context, FF4j> {
        public FF4jProvider() {
            super(FF4j.class, ff4j);
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public WebAppDescriptor configure() {
        return new WebAppDescriptor.Builder()
                .initParam(WebComponent.RESOURCE_CONFIG_CLASS, ClassNamesResourceConfig.class.getName())
                .initParam(ClassNamesResourceConfig.PROPERTY_CLASSNAMES, 
                        FF4jResource.class.getName() + ";" + FeaturesResource.class.getName() + ";"
                                + FeatureResource.class.getName() + ";"
 + FF4jProvider.class.getName() + ";"
                              + RuntimeExceptionMapper.class.getName()).build();
    }
 
    /** {@inheritDoc} */
    @Override
    public TestContainerFactory getTestContainerFactory() {
        return new GrizzlyWebTestContainerFactory();
    }
    
    /**
     * TDD.
     */
    @Test
    public void testReadAll_IT() throws ClientProtocolException, IOException {
        // Given
        assertFF4J.assertThatStoreHasSize(5);
        // ACCESS ON FF4J
        WebResource wRes = resource().path(APIPATH);
        ClientResponse httpRes = wRes.get(ClientResponse.class);
        System.out.println(String.valueOf(httpRes.getEntity(String.class)));

        // ACCESS ON STORE
        WebResource wRes2 = wRes.path("features");
        ClientResponse httpRes2 = wRes2.get(ClientResponse.class);
        System.out.println(String.valueOf(httpRes2.getEntity(String.class)));

        // ACCESS ON FEATURE
        WebResource wRes3 = wRes2.path("first");
        ClientResponse httpRes3 = wRes3.get(ClientResponse.class);
        System.out.println(String.valueOf(httpRes3.getEntity(String.class)));

    }

}
