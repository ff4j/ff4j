package org.ff4j.web.api.test.it;

import static org.ff4j.test.TestsFf4jConstants.TEST_FEATURES_FILE;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_FEATURES;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_GROUPS;
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
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Application;

import org.ff4j.FF4j;
import org.ff4j.test.AssertFf4j;
import org.ff4j.web.api.FF4jJacksonMapper;
import org.ff4j.web.api.resources.FF4jResource;
import org.ff4j.web.api.test.SampleFF4jJersey2Application;
import org.ff4j.web.api.utils.ClientHttpUtils;
import org.ff4j.web.jersey2.store.FeatureStoreHttp;
import org.ff4j.web.store.FeatureStoreHttpTestIT;
import org.glassfish.jersey.test.JerseyTest;
import org.glassfish.jersey.test.TestProperties;
import org.junit.Before;
import org.junit.BeforeClass;

import com.fasterxml.jackson.databind.ObjectMapper;

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
    protected static FF4j ff4j = new FF4j(TEST_FEATURES_FILE);
    
    /** Jackson serializer. */
    protected ObjectMapper jacksonMapper;
    
    @BeforeClass
    public static void initFF4J() {
        FeatureStoreHttpTestIT.ff4j = new FF4j(TEST_FEATURES_FILE);
    }
    
    /** {@inheritDoc} */
    @Override
    @Before
    public void setUp() throws Exception {
        super.setUp();
        if (assertFF4J == null) {
            assertFF4J = new AssertFf4j(ff4j);
        }
    }
    
    @Override
    protected Application configure() {
        // Enable logging.
        enable(TestProperties.LOG_TRAFFIC);
        enable(TestProperties.DUMP_ENTITY);
        
        // Initialisation of clientss
        setClient(ClientHttpUtils.buildJerseyClient());
        return new SampleFF4jJersey2Application(ff4j);
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
     * Convenient method to get a resource for {@link FF4jResource}
     * 
     * @return web resource
     */
    protected WebTarget resourceff4j() {
        return target().path(APIPATH);
    }

    /**
     * Convenient method to get a resource for {@link FeatureStoreHttp}
     * 
     * @return web resource
     */
    protected WebTarget resourceStore() {
        return resourceff4j().path(RESOURCE_STORE);
    }

    /**
     * Convenient method to get a resource for {@link FeaturesResource}
     * 
     * @return web resource
     */
    protected WebTarget resourceFeatures() {
        return resourceStore().path(RESOURCE_FEATURES);
    }

    /**
     * Convenient method to get a resource for {@link GroupsResource}
     * 
     * @return web resource
     */
    protected WebTarget resourceGroups() {
        return resourceStore().path(RESOURCE_GROUPS);
    }

}
