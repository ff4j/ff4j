package org.ff4j.neo4j;

import java.util.Map;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.neo4j.store.FeatureStoreNeo4J;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.factory.GraphDatabaseSettings;
import org.neo4j.test.TestGraphDatabaseFactory;

/**
 * Sample Test
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class DefaultUnitTesting implements FF4jNeo4jConstants {

    /** DataBase instance. */
    protected GraphDatabaseService graphDb;

    /**
     * Create temporary database for each unit test.
     */
    @Before
    public void prepareTestDatabase() {
       
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
        
            graphDb.execute("CREATE "
                    + " (AwesomeFeature:FF4J_FEATURE { uid:'AwesomeFeature', enable:true, description:'some desc' }),\n"
                    + " (first:FF4J_FEATURE { uid:'first',  enable:true, description:'first',  roles:['USER'] }),\n"
                    + " (ppint:FF4J_PROPERTY { name:'ppint', type:'org.ff4j.property.PropertyInt', value:'12' }),\n"
                    + " (ppdouble:FF4J_PROPERTY { name:'ppdouble', value:'12.5' }),\n"
                    + " (ppboolean:FF4J_PROPERTY { name:'ppboolean', value:'true' }),\n"
                    + " (ppstring:FF4J_PROPERTY { name:'ppstring', value:'hello' }),\n"
                    + " (ppListInt:FF4J_PROPERTY { name:'ppListInt', value:'12,13,14' }),\n"
                    + " (myLogLevel:FF4J_PROPERTY { name:'myLogLevel', value:'DEBUG', type:'org.ff4j.property.PropertyLogLevel' }),\n"
                    + " (digitValue:FF4J_PROPERTY { name:'digitValue', value:'1', type:'org.ff4j.property.PropertyInt', fixedValues: ['0','1','2','3'] }),\n"
                    + " (regionIdentifier:FF4J_PROPERTY { name:'regionIdentifier', value:'AMER', fixedValues: ['AMER','SSSS','EAST','EAST'] }),\n"
                    + " ppdouble-[:PROPERTY_OF]->first,\n"
                    + " ppboolean-[:PROPERTY_OF]->first," + "ppstring-[:PROPERTY_OF]->first,\n"
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
                   // + " (stratforth)-[:STRATEGY_OF]->first,\n"
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
   
    @After
    public void destroyTestDatabase() {
        graphDb.shutdown();
    }

    @Test
    public void getFeatureByUId() {
        FeatureStore store = new FeatureStoreNeo4J(graphDb);
        //System.out.println(store.exist("DOES-NOT-EXIST"));
        
        /*Feature first = store.read("first");
        Assert.assertNotNull(first);
        System.out.println(first.getDescription());
        System.out.println(first.getCustomProperties());*/
        
        Feature aw = store.read("first");
        System.out.println(aw.isEnable());
       
        FF4j ff4j = new FF4j();
        ff4j.setFeatureStore(store);
        
        System.out.println(ff4j.check("first"));
        Map < String, Feature > features = store.readAll();
        System.out.println(features.keySet());        
        
        /*
        try ( Transaction tx = graphDb.beginTx() )
        {
            // Database operations go here
            Result result = graphDb.execute(String.format(QUERY_CYPHER_EXISTS, "AwesomeFeature"));
            
            if (result.hasNext()) {
                Object o = result.next().get(QUERY_CYPHER_ALIAS);
                System.out.println(o);
            }
            tx.success();
        }*/
        /*System.out.println(o);
        
        Node n = null;
        try ( Transaction tx = graphDb.beginTx() )
        {
            n = graphDb.createNode();
            n.setProperty( "name", "Nancy" );
            tx.success();
        }

        // The node should have a valid id
        assertThat( n.getId(), is( greaterThan( -1L ) ) );

        // Retrieve a node by using the id of the created node. The id's and
        // property should match.
        try ( Transaction tx = graphDb.beginTx() )
        {
            Node foundNode = graphDb.getNodeById( n.getId() );
            assertThat( foundNode.getId(), is( n.getId() ) );
            assertThat( (String) foundNode.getProperty( "name" ), is( "Nancy" ) );
        }*/
        
       /*
        Node result = null;
        String uid =" first";
        ResourceIterator<Node> resultIterator = null;
        ExecutionEngine engine = new ExecutionEngine(graphDb, null);
        try ( Transaction tx = graphDb.beginTx() )
        {
            String queryString = "MERGE (n:Feature {uid: {uid}}) RETURN n";
            Map<String, Object> parameters = new HashMap<>();
            parameters.put( "uid", uid );
            resultIterator = (ResourceIterator<Node>) engine.execute( queryString, parameters ).columnAs( "n" );
            result = resultIterator.next();
            tx.success();
        }
        System.out.println(result);*/
    }
    
}
