package org.ff4j.test.store;

import java.net.UnknownHostException;
import java.util.Arrays;

import org.bson.Document;
import org.ff4j.core.FeatureStore;
import org.ff4j.mongo.store.FeatureStoreMongo;
import org.junit.Assert;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;

import com.github.fakemongo.junit.FongoRule;
import com.mongodb.MongoClient;
import com.mongodb.MongoCredential;
import com.mongodb.ServerAddress;

/*
 * #%L ff4j-store-jdbc %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import com.mongodb.client.MongoCollection;

/**
 * Unit testing of MongoDB Store.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreMongoCollectionCore1Test extends FeatureStoreTestSupport {

    /**
     * DataBase.
     */
    @Rule
    public FongoRule fongoRule = new FongoRule(false);

    /** {@inheritDoc} */
    @Override
    protected FeatureStore initStore() {
        return new FeatureStoreMongo(fongoRule.getDatabase().getCollection("ff4j"), "ff4j.xml");
        
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
     * Open real connection to MongoDB.
     *
     * @return
     *      target mongo client
     * @throws UnknownHostException
     *      exeption when creating server address
     */
    private MongoClient getMongoClient() throws UnknownHostException {
        // Given (using real connection)
        MongoCredential credential = MongoCredential.createMongoCRCredential("username", "FF4J", "password".toCharArray());
        ServerAddress adr = new ServerAddress("localhost", 22012);
        return new MongoClient(adr, Arrays.asList(credential));
    }
    
    /**
     * LazyBSONObjectList vs BasicBSONObjectList
     */
    @Test
    @Ignore
    public void emptyListAttributes() throws UnknownHostException {
        MongoCollection<Document> features = getMongoClient().getDatabase("FF4J").getCollection("feature");
        // When
        FeatureStore mongoStore = new FeatureStoreMongo(features, "ff4j.xml");
        // Then (no error)
        Assert.assertTrue(mongoStore.readAll().keySet().size() > 0);
    }
    
    
}
