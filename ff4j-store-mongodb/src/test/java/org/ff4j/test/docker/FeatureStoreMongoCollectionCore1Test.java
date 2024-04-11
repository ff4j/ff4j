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

import org.bson.Document;
import org.ff4j.core.FeatureStore;
import org.ff4j.mongo.store.FeatureStoreMongo;
import org.ff4j.test.store.FeatureStoreTestSupport;
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
import com.mongodb.client.MongoCollection;


/**
 * Unit testing of MongoDB Store.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Ignore
public class FeatureStoreMongoCollectionCore1Test extends FeatureStoreTestSupport {

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
    @Override
    protected FeatureStore initStore() {
        return new FeatureStoreMongo(getMongoClient().getDatabase("ff4j").getCollection("feature"), "ff4j.xml");

        // Could initialize this way
        //storeMongoDB.importFeaturesFromXmlFile("ff4j.xml");

        // Or this way
        // storeMongoDB.create(new Feature("AwesomeFeature", true, "some desc"));
        //storeMongoDB.create(new Feature("first", true, "description", null, Arrays.asList("USER")));
        //storeMongoDB.create(new Feature("second", false, "description", "GRP0", Arrays.asList("USER")));
        //storeMongoDB.create(new Feature("third", false, "ThirdJDBC", "GRP1", Arrays.asList("ADMINISTRATOR", "BETA-TESTER")));
        //FlippingStrategy strategy = new org.ff4j.strategy.el.ExpressionFlipStrategy();
        //strategy.init("forth", ParameterUtils.toMap("expression=third|second"));
        //storeMongoDB.create(new Feature("forth", true, "ForthJDBC", "GRP1", Arrays.asList("ADMINISTRATOR", "BETA-TESTER"),
        //        strategy));
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

    /**
     * LazyBSONObjectList vs BasicBSONObjectList
     */
    @Test
    @Ignore
    public void emptyListAttributes() {
        MongoCollection<Document> features = getMongoClient().getDatabase("ff4j").getCollection("feature");
        // When
        FeatureStore mongoStore = new FeatureStoreMongo(features, "ff4j.xml");
        // Then (no error)
        Assert.assertTrue(mongoStore.readAll().keySet().size() > 0);
    }
}
