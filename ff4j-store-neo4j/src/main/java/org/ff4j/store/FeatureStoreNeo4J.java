package org.ff4j.store;

import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;

/*
 * #%L
 * ff4j-store-neo4j
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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

import java.util.Map;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.utils.Util;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Transaction;

/**
 * Implementatino of NEO4J Store.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreNeo4J implements FeatureStore {

    /** Persistent storage. */
    private GraphDatabaseService graphDb;
    
    /** {@inheritDoc} */
    @Override
    public void enable(String featureID) {
        /*try ( Transaction tx = graphDb.beginTx() ) {
            Node firstNode = graphDb.createNode();
            firstNode.setProperty( "message", "Hello, " );
            Node secondNode = graphDb.createNode();
            secondNode.setProperty( "message", "World!" );
            Relationship relationship = firstNode.createRelationshipTo( secondNode, FF4jNeo4jRelationShips.MEMBER_OF);
            relationship.setProperty( "message", "brave Neo4j " );
            
            // Database operations go here
            tx.success();
        }*/
    }

    @Override
    public void disable(String fId) {
    }

    @Override
    public boolean exist(String featId) {
        Util.assertHasLength(featId);
        
        Result result = db.execute( "match (n {name: 'my node'}) return n, n.name" ) )
        {
            while ( result.hasNext() )
            {
                Map<String,Object> row = result.next();
                for ( Entry<String,Object> column : row.entrySet() )
                {
                    rows += column.getKey() + ": " + column.getValue() + "; ";
                }
                rows += "\n";
            }
            
        Node n = null;
        try ( Transaction tx = graphDb.beginTx() ) {
            graphDb.execute("")
            tx.success();
        }       

        // Retrieve a node by using the id of the created node. The id's and
        // property should match.
        try ( Transaction tx = graphDb.beginTx() )
        {
            Node foundNode = graphDb.getNodeById( n.getId() );
            assertThat( foundNode.getId(), is( n.getId() ) );
            assertThat( (String) foundNode.getProperty( "name" ), is( "Nancy" ) );
        }
        
        return false;
    }

    @Override
    public void create(Feature fp) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public Feature read(String featureUid) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Map<String, Feature> readAll() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void delete(String fpId) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void update(Feature fp) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void grantRoleOnFeature(String flipId, String roleName) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void removeRoleFromFeature(String flipId, String roleName) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void enableGroup(String groupName) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void disableGroup(String groupName) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public boolean existGroup(String groupName) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public Map<String, Feature> readGroup(String groupName) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void addToGroup(String featureId, String groupName) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void removeFromGroup(String featureId, String groupName) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public Set<String> readAllGroups() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public boolean isCached() {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public String getCacheProvider() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public String getCachedTargetStore() {
        // TODO Auto-generated method stub
        return null;
    }

    /**
     * Getter accessor for attribute 'graphDb'.
     *
     * @return
     *       current value of 'graphDb'
     */
    public GraphDatabaseService getGraphDb() {
        return graphDb;
    }

    /**
     * Setter accessor for attribute 'graphDb'.
     * @param graphDb
     * 		new value for 'graphDb '
     */
    public void setGraphDb(GraphDatabaseService graphDb) {
        this.graphDb = graphDb;
    }
    

}
