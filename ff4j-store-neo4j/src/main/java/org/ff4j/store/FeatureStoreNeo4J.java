package org.ff4j.store;

import java.util.HashMap;

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
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.neo4j.FF4jNeo4jConstants;
import org.ff4j.neo4j.FF4jNeo4jLabels;
import org.ff4j.neo4j.mapper.FeatureNeo4jMapper;
import org.ff4j.utils.Util;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Result;
import org.neo4j.graphdb.Transaction;

/**
 * Implementatino of NEO4J Store.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreNeo4J implements FeatureStore, FF4jNeo4jConstants {

    /** Persistent storage. */
    private GraphDatabaseService graphDb;
    
    /**
     * Default to create instances.
     */
    public FeatureStoreNeo4J() {
    }
    
    /**
     * Initialization of store
     */
    public FeatureStoreNeo4J(GraphDatabaseService myDb) {
        this.graphDb = myDb;
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean exist(String featId) {
        Util.assertHasLength(featId);
        String queryExist = String.format(FF4jNeo4jConstants.QUERY_CYPHER_EXISTS, featId);
        Result result = graphDb.execute(queryExist);
        Object count = null;
        if (result.hasNext()) {
            count = result.next().get(QUERY_CYPHER_ALIAS);
        }
        return (null != count) && (((long) count) > 0);
    }
    
    /** {@inheritDoc} */
    @Override
    public void create(Feature fp) {
        if (fp == null) {
            throw new IllegalArgumentException("Feature cannot be null nor empty");
        }
        if (exist(fp.getUid())) {
            throw new FeatureAlreadyExistException(fp.getUid());
        }
        update(fp);
    }
    
    /** {@inheritDoc} */
    @Override
    public void enable(String uid) {
        Util.assertHasLength(uid);
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        try (Transaction tx = graphDb.beginTx()) {
            graphDb.execute(String.format(FF4jNeo4jConstants.QUERY_CYPHER_ENABLE, uid));
            tx.success();
        }
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String uid) {
        Util.assertHasLength(uid);
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        try (Transaction tx = graphDb.beginTx()) {
            graphDb.execute(String.format(FF4jNeo4jConstants.QUERY_CYPHER_DISABLE, uid));
            tx.success();
        }
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String featId) {
        Util.assertHasLength(featId);

        // Parameters
        Map < String, Object > queryParameters = new HashMap<>();
        queryParameters.put("uid", featId);
        
        Feature targetFeature = null;
        try (Transaction tx = graphDb.beginTx()) {
        
            Result result = graphDb.execute(QUERY_CYPHER_READ_FEATURE,  queryParameters);
    
            // Parsing
            while (result.hasNext()) {
                Map < String, Object > response = result.next();
                // Map feature once
                if (targetFeature == null) {
                    Node nodeFeature = (Node) response.get("f");
                    targetFeature = FeatureNeo4jMapper.fromNode2Feature(nodeFeature);
                }
                
                // Map for 'all'
                Node   nodeOther = (Node) response.get("all");
                String nodeLabel = nodeOther.getLabels().iterator().next().toString();
                FF4jNeo4jLabels lblb = FF4jNeo4jLabels.valueOf(nodeLabel);
                switch(lblb) {
                    case FF4J_PROPERTY:
                        //Map < String, Object > properties = nodeOther.getAllProperties();
                        //System.out.println(properties);
                        System.out.println("FF4J_PROPERTY");
                    break;
                    case FF4J_FLIPPING_STRATEGY:
                        System.out.println("FF4J_FLIPPING_STRATEGY");
                        //nodeOther.getProperty("initParams");
                        nodeOther.getProperty("clazz");
                    break;
                    case FF4J_FEATURE_GROUP:
                        System.out.println("FF4J_FEATURE_GROUP");
                        nodeOther.getProperty("name");
                    break;
                    default:
                    break;
                }
            }
            tx.success();
        }
        return targetFeature;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
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
