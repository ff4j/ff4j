package org.ff4j.neo4j;

import org.ff4j.core.FeatureStore;
import org.ff4j.neo4j.store.FeatureStoreNeo4J;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.factory.GraphDatabaseSettings;
import org.neo4j.test.TestGraphDatabaseFactory;

public class FeatureStoreNeo4jSchemaTest {
    
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
    }

    @AfterClass
    public static void destroyTestDatabase() {
        graphDb.shutdown();
    }
    
    @Test
    public void testCreateSchema() {
        // Given
        FeatureStore fStore = new FeatureStoreNeo4J(graphDb);
        // WHEN
        fStore.createSchema();
        fStore.createSchema();
        // No error here
    }    
}
