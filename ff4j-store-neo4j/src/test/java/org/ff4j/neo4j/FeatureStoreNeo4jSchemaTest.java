package org.ff4j.neo4j;

/*-
 * #%L
 * ff4j-store-neo4j
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

import org.ff4j.core.FeatureStore;
import org.ff4j.neo4j.store.FeatureStoreNeo4J;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.harness.Neo4j;
import org.neo4j.harness.Neo4jBuilders;

public class FeatureStoreNeo4jSchemaTest {
    
    /** DataBase instance. */
    protected static GraphDatabaseService graphDb;

    /** Embedded Neo4j test harness. */
    protected static Neo4j neo4j;

    /**
     * Create temporary database for each unit test.
     */
    @BeforeClass
    public static void prepareTestDatabase() {
       
        // Embedded DATABASE
        neo4j = Neo4jBuilders.newInProcessBuilder().withDisabledServer().build();
        graphDb = neo4j.defaultDatabaseService();
    }

    @AfterClass
    public static void destroyTestDatabase() {
        neo4j.close();
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
