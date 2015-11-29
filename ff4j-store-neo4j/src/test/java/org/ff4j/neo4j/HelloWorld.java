package org.ff4j.neo4j;

import java.util.HashMap;
import java.util.Map;

import org.ff4j.core.Feature;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.neo4j.cypher.javacompat.ExecutionEngine;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.ResourceIterator;
import org.neo4j.graphdb.Transaction;
import org.neo4j.graphdb.factory.GraphDatabaseSettings;
import org.neo4j.test.TestGraphDatabaseFactory;

/**
 * Sample Test
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class HelloWorld implements FF4jNeo4jConstants {

    /** DataBase instance. */
    protected GraphDatabaseService graphDb;

    /**
     * Create temporary database for each unit test.
     */
    @Before
    public void prepareTestDatabase() {
        // Create Embedded database
        graphDb = new TestGraphDatabaseFactory().newImpermanentDatabaseBuilder()
                .setConfig(GraphDatabaseSettings.nodestore_mapped_memory_size, "10M")
                .setConfig(GraphDatabaseSettings.string_block_size, "60")
                .setConfig(GraphDatabaseSettings.array_block_size, "300").newGraphDatabase();
        
        /* Ensure unicity of feature uid
        graphDb.schema().constraintFor( DynamicLabel.label( "Feature" ) )
                .assertPropertyIsUnique( "uid" )
                .create();
        */
        
        /*
        IndexDefinition indexDefinition;
        try ( Transaction tx = graphDb.beginTx() )
        {
            Schema schema = graphDb.schema();
            indexDefinition = schema.indexFor( DynamicLabel.label( "User" ) )
                    .on( "username" )
                    .create();
            tx.success();
        }
        */
    }

    /**
     * Shutdown neo4j database
     *
    @After
    public void destroyTestDatabase() {
        graphDb.shutdown();
    }

    @Test
    public void getByIuid() {
        Node result = null;
        String uid =" first";
        ResourceIterator<Node> resultIterator = null;
        ExecutionEngine engine = new ExecutionEngine( graphDb);
        try ( Transaction tx = graphDb.beginTx() )
        {
            String queryString = "MERGE (n:Feature {uid: {uid}}) RETURN n";
            Map<String, Object> parameters = new HashMap<>();
            parameters.put( "uid", uid );
            resultIterator = engine.execute( queryString, parameters ).columnAs( "n" );
            result = resultIterator.next();
            tx.success();
        }
        System.out.println(result);
    }
    
    @Test
    public void shouldCreateNode() {
        
        Node nodeFeature;
        Feature feature = new Feature("toto", true, "Sample toto");
        try (Transaction tx = graphDb.beginTx()) {
            nodeFeature = graphDb.createNode(FF4jNeo4jLabels.FEATURE);
            nodeFeature.setProperty(P_FEATURE_UID, feature.getUid());
            nodeFeature.setProperty(P_FEATURE_DESCRIPTION, feature.getDescription());
            nodeFeature.setProperty(P_FEATURE_ENABLE, feature.isEnable());
            
            tx.success();
        }

        // The node should have a valid id
        assertThat(nodeFeature.getId(), is(greaterThan(-1L)));

        // Retrieve a node by using the id of the created node. The id's and
        // property should match.
        try (Transaction tx = graphDb.beginTx()) {
            Node foundNode = graphDb.getNodeById(nodeFeature.getId());
            assertThat(foundNode.getId(), is(nodeFeature.getId()));
            assertThat((String) foundNode.getProperty("uid"), is("toto"));
        }
    }*/

}
