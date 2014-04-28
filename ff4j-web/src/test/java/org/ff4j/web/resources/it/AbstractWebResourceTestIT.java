package org.ff4j.web.resources.it;

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
import javax.ws.rs.core.Context;
import javax.ws.rs.ext.Provider;

import org.ff4j.FF4j;
import org.ff4j.test.AssertFf4j;
import org.ff4j.test.TestsFf4jConstants;
import org.ff4j.web.api.FF4jWebApiConstants;
import org.ff4j.web.resources.FF4jResource;
import org.ff4j.web.resources.FeatureResource;
import org.ff4j.web.resources.FeaturesResource;
import org.ff4j.web.resources.GroupResource;
import org.ff4j.web.resources.GroupsResource;
import org.ff4j.web.resources.RuntimeExceptionMapper;
import org.ff4j.web.store.FeatureStoreHttp;
import org.junit.Before;

import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.api.core.ClassNamesResourceConfig;
import com.sun.jersey.spi.container.servlet.WebComponent;
import com.sun.jersey.spi.inject.SingletonTypeInjectableProvider;
import com.sun.jersey.test.framework.JerseyTest;
import com.sun.jersey.test.framework.WebAppDescriptor;
import com.sun.jersey.test.framework.spi.container.TestContainerFactory;
import com.sun.jersey.test.framework.spi.container.grizzly2.web.GrizzlyWebTestContainerFactory;

/**
 * Superclass for testing web resources.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public abstract class AbstractWebResourceTestIT extends JerseyTest implements TestsFf4jConstants, FF4jWebApiConstants {
    
    /** Relative resource. */
    public final static String APIPATH = FF4jResource.class.getAnnotation(Path.class).value();

    /** Assert for this ff4j instance. */
    protected static AssertFf4j assertFF4J;

    /** Current ff4j. */
    protected static FF4j ff4j = new FF4j(TEST_FEATURES_FILE);

    /** {@inheritDoc} */
    @Override
    @Before
    public void setUp() throws Exception {
        if (assertFF4J == null) {
            assertFF4J = new AssertFf4j(ff4j);
        }
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
        StringBuilder jerseyContext = new StringBuilder();
        jerseyContext.append(FF4jProvider.class.getName() + ";");
        jerseyContext.append(FF4jResource.class.getName() + ";");
        jerseyContext.append(FeaturesResource.class.getName() + ";");
        jerseyContext.append(FeatureResource.class.getName() + ";");
        jerseyContext.append(GroupsResource.class.getName() + ";");
        jerseyContext.append(GroupResource.class.getName() + ";");
        jerseyContext.append(RuntimeExceptionMapper.class.getName());
        return new WebAppDescriptor.Builder()
                .initParam(WebComponent.RESOURCE_CONFIG_CLASS, ClassNamesResourceConfig.class.getName())
                .initParam(ClassNamesResourceConfig.PROPERTY_CLASSNAMES, jerseyContext.toString()).build();
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
