package org.ff4j.neo4j;

import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.factory.GraphDatabaseSettings;
import org.neo4j.test.TestGraphDatabaseFactory;

/**
 * Sample Test
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class DefaultUnitTesting {

    /** DataBase instance. */
    protected GraphDatabaseService graphDb;

    /**
     * Create temporary database for each unit test.
     */
    @Before
    public void prepareTestDatabase() {
       
        graphDb = new TestGraphDatabaseFactory().newImpermanentDatabaseBuilder()
                .setConfig(GraphDatabaseSettings.string_block_size, "60")
                .setConfig(GraphDatabaseSettings.array_block_size, "300").newGraphDatabase();
        
        try (Transaction tx = graphDb.beginTx() ) {
            
            graphDb.schema().constraintFor(FF4jNeo4jLabels.FF4J_FEATURE)//
                    .assertPropertyIsUnique( "uid" )//
                    .create();
            
            graphDb.schema().constraintFor(FF4jNeo4jLabels.FF4J_FEATURE_GROUP)//
                    .assertPropertyIsUnique( "groupName" )//
                    .create();
          
            graphDb.schema().constraintFor(FF4jNeo4jLabels.PROPERTY)//
                    .assertPropertyIsUnique( "name" )//
                    .create();
            
            /*
            graphDb.schema().indexFor(FF4jNeo4jLabels.FEATURE)//
                    .on(FeatureNeo4jMapper.attributeUID)//
                    .create();
            */
            
            tx.success();
        }
    }
   
    @After
    public void destroyTestDatabase() {
        graphDb.shutdown();
    }

    @Test
    public void getByIuid() {
        
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
        }
        
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
