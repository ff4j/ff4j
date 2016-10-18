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
import org.junit.AfterClass;
import org.junit.Assert;
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
        // No error here even if calling the method twice
        Assert.assertNotNull(fStore);
        
    }    
}
