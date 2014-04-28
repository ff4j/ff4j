package org.ff4j.web.store;

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

import org.ff4j.core.FeatureStore;
import org.ff4j.test.store.AbstractStoreJUnitTest;
import org.junit.AfterClass;
import org.junit.BeforeClass;

import com.sun.jersey.test.framework.JerseyTest;

/**
 * Unitary test for {@link FeatureStoreHttp} on Grizzly server.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreHttpTest extends AbstractStoreJUnitTest {

    /** Jersy Test */
    private static JerseyTest jt = null;
    
    /**
     * Start Server Grizzly before tests on remote FeatureStore.
     */
    @BeforeClass
    public static void initializingInMemory() throws Exception {
        jt = new FeatureStoreHttpTestIT();
        jt.setUp();
    }

    /** {@inheritDoc} */
    @Override
    protected FeatureStore initStore() {
        FeatureStoreHttp fst = new FeatureStoreHttp("http://localhost:9998/ff4j");
        return fst;
    }

    /**
     * Start Server Grizzly before tests on FeatureStore
     * 
     * @throws Exception
     */
    @AfterClass
    public static void shutDownInMemeoryServer() throws Exception {
        jt.tearDown();
    }

}
