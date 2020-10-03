package org.ff4j.test.docker;

import com.mongodb.ConnectionString;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import org.ff4j.mongo.mapper.PropertyDocumentBuilder;
import org.ff4j.mongo.store.PropertyStoreMongo;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.*;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.MongoDBContainer;
import org.testcontainers.containers.wait.strategy.Wait;

import java.time.Duration;


/**
 * Unit testing of MongoDB Store.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class PropertyStoreMongoCollectionCore1Test extends PropertyStoreTestSupport {

    private static final int MONGO_PORT = 27017;

    /**
     * DataBase.
     */
    @ClassRule
    public static GenericContainer<?> mongoDBContainer = new GenericContainer<>("mongo:latest").withExposedPorts(MONGO_PORT);

    @BeforeClass
    public static void startDocker() {
        mongoDBContainer = new MongoDBContainer();
        mongoDBContainer.start();
        mongoDBContainer.waitingFor(Wait.forListeningPort().withStartupTimeout(Duration.ofSeconds(180L)));
    }

    @AfterClass
    public static void stopDocker() {
        mongoDBContainer.stop();
    }

    /**
     * {@inheritDoc}
     */
    protected PropertyStore initPropertyStore() {
        PropertyStoreMongo propertyStoreMongo = new PropertyStoreMongo(getMongoClient());
        propertyStoreMongo.importPropertiesFromXmlFile("test-ff4j-features.xml");

        return propertyStoreMongo;
    }

    /**
     * Open connection to MongoDB container.
     *
     * @return target mongo client
     */
    private MongoClient getMongoClient() {
        ConnectionString connectionString = new ConnectionString("mongodb://" + mongoDBContainer.getContainerIpAddress() + ":" + mongoDBContainer.getMappedPort(MONGO_PORT));
        return MongoClients.create(connectionString);
    }

    @Test
    public void testInit() {
        PropertyDocumentBuilder pod = new PropertyDocumentBuilder();
        Assert.assertNotNull(pod.getDescription("a"));
        Assert.assertNotNull(pod.getType("a"));
        Assert.assertNotNull(pod.getFixedValues("a"));
    }
}
