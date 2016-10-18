package org.ff4j.neo4j;

/*
 * #%L
 * ff4j-store-neo4j
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


import org.ff4j.core.Feature;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.neo4j.store.FeatureStoreNeo4J;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyString;
import org.ff4j.strategy.PonderationStrategy;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.factory.GraphDatabaseSettings;
import org.neo4j.test.TestGraphDatabaseFactory;

import static org.ff4j.neo4j.FF4jNeo4jConstants.*;

public class FeatureStoreNeo4jLimitTest {

    /** DataBase instance. */
    protected static GraphDatabaseService graphDb;
    
    /** store. **/
    protected FeatureStoreNeo4J testedStore = new FeatureStoreNeo4J(graphDb);

    /**
     * Create temporary database for each unit test.
     */
    @BeforeClass
    public static void prepareTestDatabase() {
       
        // Embedded DATABASE
        graphDb = new TestGraphDatabaseFactory().newImpermanentDatabaseBuilder()
                .setConfig(GraphDatabaseSettings.string_block_size, "300")
                .setConfig(GraphDatabaseSettings.array_block_size, "300")
                .newGraphDatabase();
        
        // Init Schema in a first Transaction
        try (Transaction tx = graphDb.beginTx() ) {
            graphDb.schema().constraintFor(FF4jNeo4jLabels.FF4J_FEATURE)//
                    .assertPropertyIsUnique(NODEFEATURE_ATT_UID)//
                    .create();
            graphDb.schema().constraintFor(FF4jNeo4jLabels.FF4J_FEATURE_GROUP)//
                    .assertPropertyIsUnique(NODEGROUP_ATT_NAME )//
                    .create();
            // An index is automatically place
            // graphDb.schema().indexFor(FF4jNeo4jLabels.FF4J_FEATURE).on(NODEFEATURE_ATT_UID).create();
            tx.success();
        }
    }
   
    @Test
    public void testDefaultInit() {
        Assert.assertNotNull(new FeatureStoreNeo4J());
    }
    
    @Test
    public void testDeleteLastElement() {
        try (Transaction tx= graphDb.beginTx() ) {
            graphDb.execute("CREATE "
                    + " (g0:FF4J_FEATURE_GROUP { name:'g0' }),\n"
                    + " (f1:FF4J_FEATURE { uid:'f1', enable:false, description:'second', roles:['USER'] }),\n"
                    + " (f1)-[:MEMBER_OF]->(g0);");
            tx.success();
        }
        // Remove last
        testedStore.delete("f1");
        Assert.assertFalse(testedStore.existGroup("g0"));
    }
    
    @Test
    public void testChangeGroup() {
        try (Transaction tx= graphDb.beginTx() ) {
            graphDb.execute("CREATE "
                    + " (h0:FF4J_FEATURE_GROUP { name:'h0' }),\n"
                    + " (h1:FF4J_FEATURE { uid:'h1', enable:false, description:'second', roles:['USER'] }),\n"
                    + " (h1)-[:MEMBER_OF]->(h0);");
            tx.success();
        }
        Feature f1 = testedStore.read("h1");
        Property<?> newP = new PropertyString("ppp", "vvv");
        newP.setDescription("a description");
        f1.addProperty(newP);
        
        FlippingStrategy fs = new PonderationStrategy(0.1);
        fs.getInitParams().put("p1", "v1");
        fs.getInitParams().put("p2","v2");
        f1.setFlippingStrategy(fs);
        
        f1.setGroup("g2");
        testedStore.update(f1);
        Assert.assertEquals("g2", testedStore.read("h1").getGroup());
        
        f1.getFlippingStrategy().getInitParams().put("p3", "v3");
        testedStore.update(f1);
        
        
        testedStore.setGraphDb(testedStore.getGraphDb());
    }
    
    @Test(expected = GroupNotFoundException.class)
    public void testRemoveFromInvalidGroup() {
        try (Transaction tx= graphDb.beginTx() ) {
            graphDb.execute("CREATE (x1:FF4J_FEATURE { uid:'x1', enable:false });");
            tx.success();
        }
        testedStore.removeFromGroup("x1", "invalidGroup");
    }
    
    
    @AfterClass
    public static void destroyTestDatabase() {
        graphDb.shutdown();
    }
    

}
