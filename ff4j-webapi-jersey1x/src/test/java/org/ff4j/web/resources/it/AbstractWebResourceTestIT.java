package org.ff4j.web.resources.it;

import static org.ff4j.test.TestsFf4jConstants.TEST_FEATURES_FILE;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_FEATURES;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_GROUPS;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_PROPERTIES;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_PROPERTYSTORE;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_STORE;

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

import javax.ws.rs.Path;

import org.ff4j.FF4j;
import org.ff4j.test.AssertFf4j;
import org.ff4j.web.ApiConfig;
import org.ff4j.web.api.FF4JApiApplication;
import org.ff4j.web.api.FF4jJacksonMapper;
import org.ff4j.web.api.resources.FF4jResource;
import org.ff4j.web.jersey1.store.FeatureStoreHttp;
import org.junit.Before;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.api.client.config.ClientConfig;
import com.sun.jersey.api.client.config.DefaultClientConfig;
import com.sun.jersey.api.json.JSONConfiguration;
import com.sun.jersey.spi.container.servlet.WebComponent;
import com.sun.jersey.test.framework.JerseyTest;
import com.sun.jersey.test.framework.WebAppDescriptor;
import com.sun.jersey.test.framework.spi.container.TestContainerFactory;
import com.sun.jersey.test.framework.spi.container.grizzly2.web.GrizzlyWebTestContainerFactory;

/**
 * Superclass for testing web resources.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public abstract class AbstractWebResourceTestIT extends JerseyTest {
    
    /** Relative resource. */
    public final static String APIPATH = FF4jResource.class.getAnnotation(Path.class).value();

    /** Assert for this ff4j instance. */
    protected static AssertFf4j assertFF4J;

    /** Current ff4j. */
    public static FF4j ff4j = new FF4j(TEST_FEATURES_FILE);
    
    /** Jackson serializer. */
    protected ObjectMapper jacksonMapper;
    
    /** {@inheritDoc} */
    @Override
    @Before
    public void setUp() throws Exception {
        if (assertFF4J == null) {
            assertFF4J = new AssertFf4j(ff4j);
        }
    }

    /**
     * Serialize with custom jackson.
     * @param o
     *      current object
     * @return
     *      serialize
     */
    protected String toJson(Object o) {
        try {
            if (jacksonMapper == null) {
                jacksonMapper = new FF4jJacksonMapper().getContext(getClass());
            }
            return jacksonMapper.writeValueAsString(o);
        } catch (Exception e) {
            throw new IllegalArgumentException("Cannot serialize", e);
        }
    }
    
    /**
     * Utilization of out-of-thr-box jersey configuration.
     *
     * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
     */
    public static class SimpleFF4jProvider extends FF4JApiApplication {

        /** {@inheritDoc} */
        @Override
        public ApiConfig getApiConfig() {
            return new ApiConfig(ff4j);
        }
        
    }

    /** {@inheritDoc} */
    @Override
    public WebAppDescriptor configure() {
        ClientConfig cc = new DefaultClientConfig();
        cc.getFeatures().put(JSONConfiguration.FEATURE_POJO_MAPPING, Boolean.TRUE);
        return new WebAppDescriptor.Builder()
                .initParam(WebComponent.APPLICATION_CONFIG_CLASS, SimpleFF4jProvider.class.getName())//
                .clientConfig(cc).build();
    }

    /** {@inheritDoc} */
    @Override
    public TestContainerFactory getTestContainerFactory() {
        return new GrizzlyWebTestContainerFactory();
    }

    /**
     * Convenient method to get a resource for {@link FF4jResource}
     * 
     * @return web resource
     */
    protected WebResource resourceff4j() {
        return resource().path(APIPATH);
    }

    /**
     * Convenient method to get a resource for {@link FeatureStoreHttp}
     * 
     * @return web resource
     */
    protected WebResource resourceStore() {
        return resourceff4j().path(RESOURCE_STORE);
    }
    
    /**
     * Convenient method to get a resource for {@link FeatureStoreHttp}
     * 
     * @return web resource
     */
    protected WebResource rscPropertyStore() {
        return resourceff4j().path(RESOURCE_PROPERTYSTORE);
    }
    
    /**
     * Convenient method to get a resource for {@link FeatureStoreHttp}
     * 
     * @return web resource
     */
    protected WebResource rscProperties() {
        return rscPropertyStore().path(RESOURCE_PROPERTIES);
    }

    /**
     * Convenient method to get a resource for {@link FeaturesResource}
     * 
     * @return web resource
     */
    protected WebResource resourceFeatures() {
        return resourceStore().path(RESOURCE_FEATURES);
    }

    /**
     * Convenient method to get a resource for {@link GroupsResource}
     * 
     * @return web resource
     */
    protected WebResource resourceGroups() {
        return resourceStore().path(RESOURCE_GROUPS);
    }

}
