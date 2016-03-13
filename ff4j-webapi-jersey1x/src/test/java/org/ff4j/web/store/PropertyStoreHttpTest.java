package org.ff4j.web.store;

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


import org.ff4j.FF4j;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.ff4j.web.jersey1.store.FeatureStoreHttp;
import org.ff4j.web.jersey1.store.PropertyStoreHttp;
import org.junit.AfterClass;
import org.junit.BeforeClass;

import com.sun.jersey.test.framework.JerseyTest;

/**
 * Unitary test for {@link FeatureStoreHttp} on Grizzly server.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class PropertyStoreHttpTest extends PropertyStoreTestSupport {

    /** Jersy Test */
    private static JerseyTest jt = null;
    
    /**
     * Start Server Grizzly before tests on remote FeatureStore.
     */
    @BeforeClass
    public static void initializingInMemory() throws Exception {
        // Reinit FF4J
        FeatureStoreHttpTestIT.ff4j = new FF4j("test-ff4j-features.xml");
        jt = new PropertyStoreHttpTestIT();
        jt.setUp();
    }
    
    /** {@inheritDoc} */
    protected PropertyStore initPropertyStore() {
        PropertyStoreHttp fst = new PropertyStoreHttp("http://localhost:9998/ff4j");
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
