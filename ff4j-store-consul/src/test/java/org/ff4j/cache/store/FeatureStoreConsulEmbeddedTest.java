package org.ff4j.cache.store;

/*
 * #%L
 * ff4j-store-consul
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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


import org.ff4j.consul.ConsulConnection;
import org.ff4j.consul.store.FeatureStoreConsul;
import org.ff4j.core.FeatureStore;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;

import com.orbitz.consul.Consul;
import com.pszymczyk.consul.ConsulProcess;
import com.pszymczyk.consul.ConsulStarterBuilder;

/**
 * Test to work with Consul as a store.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Ignore
public class FeatureStoreConsulEmbeddedTest extends FeatureStoreTestSupport {
    
    /** Initialisation of embedded consul. */
    private static ConsulProcess consulProcess;

    @BeforeClass
    public static void setup() {
        String customConfiguration = "{ \"datacenter\": \"test-dc\"," +                    
                    "\"log_level\": \"INFO\"," +
                    "\"node_name\": \"ff4j-embedded\"," +
                    "\"ports\": { \"http\": 8800  }" + "}";
        
        consulProcess = ConsulStarterBuilder.consulStarter()
                                            .withCustomConfig(customConfiguration)
                                            .build().start();    
    }

    @AfterClass
    public static void cleanup() throws Exception {
        consulProcess.close();
    }
    
    /** {@inheritDoc} */
    protected FeatureStore initStore() {
        Consul c = Consul.builder().withUrl("http://localhost:8800").build();
        ConsulConnection connection = new ConsulConnection(c);
        FeatureStoreConsul ehcacheStore = new FeatureStoreConsul(connection);
        ehcacheStore.importFeaturesFromXmlFile("ff4j.xml");
        return ehcacheStore;
    }

}
