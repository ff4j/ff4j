package org.ff4j.cache.store;

import org.ff4j.consul.ConsulConnection;
import org.ff4j.consul.store.PropertyStoreConsul;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
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
public class PropertyStoreConsulEmbeddedTest  extends PropertyStoreTestSupport {
    
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
    protected PropertyStore initPropertyStore() {
        Consul c = Consul.builder().withUrl("http://localhost:8800").build();
        ConsulConnection  connection = new ConsulConnection(c);
        PropertyStoreConsul consulStore = new PropertyStoreConsul(connection);
        consulStore.importPropertiesFromXmlFile("ff4j.xml");
        return consulStore;
    }
}
