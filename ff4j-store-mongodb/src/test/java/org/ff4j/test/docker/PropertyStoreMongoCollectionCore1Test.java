package org.ff4j.test.docker;

/*-
 * #%L
 * ff4j-store-mongodb
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import java.time.Duration;

import org.ff4j.mongo.mapper.PropertyDocumentBuilder;
import org.ff4j.mongo.store.PropertyStoreMongo;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Ignore;
import org.junit.Test;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.MongoDBContainer;
import org.testcontainers.containers.wait.strategy.Wait;

import com.mongodb.ConnectionString;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;


/**
 * Unit testing of MongoDB Store.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Ignore
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
