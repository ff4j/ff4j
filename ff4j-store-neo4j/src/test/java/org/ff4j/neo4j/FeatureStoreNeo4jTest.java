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


import org.ff4j.core.FeatureStore;
import org.ff4j.neo4j.store.FeatureStoreNeo4J;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.factory.GraphDatabaseSettings;
import org.neo4j.test.TestGraphDatabaseFactory;

import static org.ff4j.neo4j.FF4jNeo4jConstants.*;

/**
 * Unit Testing for Neo4j store.
 * 
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FeatureStoreNeo4jTest extends FeatureStoreTestSupport {

    /** DataBase instance. */
    protected static GraphDatabaseService graphDb;

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
        
        // Create Data in a second Transaction
        try (Transaction tx2= graphDb.beginTx() ) {
        
            graphDb.execute("CREATE (AwesomeFeature:FF4J_FEATURE { uid:'AwesomeFeature', enable:true, description:'some desc' }),\n"
                    + " (first:FF4J_FEATURE { uid:'first',  enable:true, description:'first',  roles:['USER'] }),\n"
                    + " (ppint:FF4J_FEATURE_PROPERTY { name:'ppint', type:'org.ff4j.property.PropertyInt', value:'12' }),\n"
                    + " (ppdouble:FF4J_FEATURE_PROPERTY { name:'ppdouble', value:'12.5' }),\n"
                    + " (ppboolean:FF4J_FEATURE_PROPERTY { name:'ppboolean', value:'true' }),\n"
                    + " (ppstring:FF4J_FEATURE_PROPERTY { name:'ppstring', value:'hello' }),\n"
                    + " (ppListInt:FF4J_FEATURE_PROPERTY { name:'ppListInt', value:'12,13,14' }),\n"
                    + " (myLogLevel:FF4J_FEATURE_PROPERTY { name:'myLogLevel', value:'DEBUG', type:'org.ff4j.property.PropertyLogLevel' }),\n"
                    + " (digitValue:FF4J_FEATURE_PROPERTY { name:'digitValue', value:'1', type:'org.ff4j.property.PropertyInt', fixedValues: ['0','1','2','3'] }),\n"
                    + " (regionIdentifier:FF4J_FEATURE_PROPERTY { name:'regionIdentifier', value:'AMER', fixedValues: ['AMER','SSSS','EAST','EAST'] }),\n"
                    + " ppint-[:PROPERTY_OF]->first,\n"
                    + " ppdouble-[:PROPERTY_OF]->first,\n"
                    + " ppboolean-[:PROPERTY_OF]->first," 
                    + " ppstring-[:PROPERTY_OF]->first,\n"
                    + " ppListInt-[:PROPERTY_OF]->first,\n"
                    + " myLogLevel-[:PROPERTY_OF]->first,\n"
                    + " digitValue-[:PROPERTY_OF]->first,\n" 
                    + " regionIdentifier-[:PROPERTY_OF]->first,\n"

                    + " (GRP0:FF4J_FEATURE_GROUP { name:'GRP0' }),\n"
                    + " (second:FF4J_FEATURE { uid:'second', enable:false, description:'second', roles:['USER'] }),\n"
                    + " (second)-[:MEMBER_OF]->(GRP0),\n"

                    + " (GRP1:FF4J_FEATURE_GROUP { name:'GRP1' }),\n"
                    + " (third:FF4J_FEATURE { uid:'third', enable:false, description:'third', roles:['ADMINISTRATOR', 'BETA-TESTER'] }),\n"
                    + " (third)-[:MEMBER_OF]->(GRP1),\n"

                    + " (forth:FF4J_FEATURE { uid:'forth', enable:true, description:'forth', roles:['ADMINISTRATOR', 'BETA-TESTER'] }),\n"
                    + " (stratforth:FF4J_FLIPPING_STRATEGY { initParams: [ 'expression=third|second' ], type: 'org.ff4j.strategy.el.ExpressionFlipStrategy'}),\n"
                    + " (stratforth)-[:STRATEGY_OF]->forth,\n" 
                    + " (forth)-[:MEMBER_OF]->(GRP1),\n"

                    + " (a:FF4J_PROPERTY { name:'a', value:'AMER', fixedValues: ['AMER','EAST','EAST','EAST'] }),\n"
                    + " (b:FF4J_PROPERTY { name:'b', value:'12' }),\n" 
                    + " (c:FF4J_PROPERTY { name:'c', value:'12.5' }),\n"
                    + " (d:FF4J_PROPERTY { name:'d', value:'true' }),\n" 
                    + " (e:FF4J_PROPERTY { name:'e', value:'hello' }),\n"
                    + " (f:FF4J_PROPERTY { name:'f', value:'12,13,14' }),\n"
                    + " (g:FF4J_PROPERTY { name:'g', value:'DEBUG', type:'org.ff4j.property.PropertyLogLevel'  });");
            
            tx2.success();
        }
    }
   
    @AfterClass
    public static void destroyTestDatabase() {
        graphDb.shutdown();
    }
    
    /** {@inheritDoc} */
    @Override
    protected FeatureStore initStore() {
        return new FeatureStoreNeo4J(graphDb);
    }

}
