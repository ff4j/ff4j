package org.ff4j.cache.store;

import org.ff4j.consul.ConsulConnection;
import org.ff4j.consul.store.FeatureStoreConsul;
import org.ff4j.core.FeatureStore;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.junit.AfterClass;
import org.junit.BeforeClass;

import com.orbitz.consul.Consul;
import com.pszymczyk.consul.ConsulProcess;
import com.pszymczyk.consul.ConsulStarterBuilder;

/**
 * Test to work with Redis as a store.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreConsulEmbeddedTest extends FeatureStoreTestSupport {
    
    /** Initialisation of embedded consul. */
    private static ConsulProcess consulProcess;

    @BeforeClass
    public static void setup() {
        String customConfiguration = "{ \"datacenter\": \"test-dc\"," +                    
                    "\"log_level\": \"INFO\"," +
                    "\"node_name\": \"ff4j-embedded\"," +
                    "\"ports\": {" +
                        "\"http\": 8800," +
                        "\"dns\": 64294," +
                        "\"rpc\": 64295," +
                        "\"serf_lan\": 64296," +
                        "\"serf_wan\": 64297," +
                        "\"server\": 64298 }" 
                    + "}";
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
