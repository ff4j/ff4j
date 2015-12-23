package org.ff4j.neo4j.store;

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

import static org.ff4j.neo4j.mapper.FeatureNeo4jMapper.fromNode2Feature;
import static org.ff4j.neo4j.mapper.FeatureNeo4jMapper.fromNode2FlippingStrategy;
import static org.ff4j.neo4j.mapper.FeatureNeo4jMapper.fromNode2Property;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.neo4j.FF4jNeo4jConstants;
import org.ff4j.neo4j.FF4jNeo4jLabels;
import org.ff4j.property.AbstractProperty;
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
        Map < String, Object > queryParameters = new HashMap<>();
        queryParameters.put("uid", featId);
        Result result = graphDb.execute(QUERY_CYPHER_EXISTS,  queryParameters);
        Object count = null;
        if (result.hasNext()) {
            count = result.next().get(QUERY_CYPHER_ALIAS);
        }
        return (null != count) && (((long) count) > 0);
    }
    
    /** {@inheritDoc} */
    @Override
    public void enable(String uid) {
        Util.assertHasLength(uid);
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        try (Transaction tx = graphDb.beginTx()) {
            Map < String, Object > queryParameters = new HashMap<>();
            queryParameters.put("uid", uid);
            graphDb.execute(QUERY_CYPHER_ENABLE,  queryParameters);
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
            Map < String, Object > queryParameters = new HashMap<>();
            queryParameters.put("uid", uid);
            graphDb.execute(QUERY_CYPHER_ENABLE,  queryParameters);
            tx.success();
        }
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String featId) {
        Util.assertHasLength(featId);
        Feature targetFeature = null;
        try (Transaction tx = graphDb.beginTx()) {
            // Build Query
            Map < String, Object > queryParameters = new HashMap<>();
            queryParameters.put("uid", featId);
            Result result = graphDb.execute(QUERY_CYPHER_READ_FEATURE,  queryParameters);
            while (result.hasNext()) {
                Map < String, Object > response = result.next();
                if (targetFeature == null) {
                    Node nodeFeature = (Node) response.get("f");
                    targetFeature = fromNode2Feature(nodeFeature);
                }
                addNeighBour2Feature(targetFeature, (Node) response.get("all"));
            }
            tx.success();
        }
        return targetFeature;
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        Map<String, Feature> allFeatures = new HashMap<>();
        try (Transaction tx = graphDb.beginTx()) {
            Result result = graphDb.execute(QUERY_CYPHER_READ_ALL);
            while (result.hasNext()) {
                Map < String, Object > response = result.next();
                Node nodeFeature = (Node) response.get("f");
                String uid = (String) nodeFeature.getProperty(NODEFEATURE_ATT_UID);
                System.out.println(uid);
                // first time meeting this uid, create core Feature object from 'f'
                if (!allFeatures.containsKey(uid)) {
                    Feature currentFeature = fromNode2Feature(nodeFeature);
                    allFeatures.put(currentFeature.getUid(), currentFeature);
                }
                addNeighBour2Feature(allFeatures.get(uid), (Node) response.get("all"));
            }
            tx.success();
        }          
        return allFeatures;
    }
    
    /**
     * Parse node related to feature and create sub component.
     *
     * @param targetFeature
     *      current feature
     * @param nodeOther
     *      current neighbour
     */
    private void addNeighBour2Feature(Feature targetFeature, Node nodeOther) {
        Util.assertNotNull(targetFeature);
        if (nodeOther != null) {
            String nodeLabel = nodeOther.getLabels().iterator().next().toString();
            FF4jNeo4jLabels lblb = FF4jNeo4jLabels.valueOf(nodeLabel);
            switch(lblb) {
                case FF4J_FLIPPING_STRATEGY:
                    FlippingStrategy fs = fromNode2FlippingStrategy(targetFeature.getUid(), nodeOther);
                    targetFeature.setFlippingStrategy(fs);
                break;
                
                case FF4J_PROPERTY:
                    AbstractProperty<?> ap = fromNode2Property(targetFeature.getUid(), nodeOther);
                    targetFeature.getCustomProperties().put(ap.getName(), ap);
                break;
                
                case FF4J_FEATURE_GROUP:
                    targetFeature.setGroup((String) nodeOther.getProperty(NODEGROUP_ATT_NAME));
                break;
                
                default:
                break;
            }
        }
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
    public void delete(String uid) {
        Util.assertHasLength(uid);
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        
        // Parameter
        Map < String, Object > paramUID = new HashMap<>();
        paramUID.put("uid", uid);
        
        try (Transaction tx = graphDb.beginTx()) {
            
            // Delete Flipping Strategy if it exists
            graphDb.execute(QUERY_CYPHER_DELETE_STRATEGY_FEATURE,  paramUID);
            
            // Delete Related Property if exist
            graphDb.execute(QUERY_CYPHER_DELETE_PROPERTIES_FEATURE,  paramUID);

            // Check group            
            Result result = graphDb.execute(QUERY_CYPHER_GETGROUPNAME, paramUID);
            if (result.hasNext()) {
                String groupName = (String) result.next().get("GROUPNAME");
                
                Map < String, Object > paramGroupName = new HashMap<>();
                paramGroupName.put("groupName", groupName);
                result = graphDb.execute(QUERY_CYPHER_COUNT_FEATURE_OF_GROUP, paramGroupName);
                if (result.hasNext()) {
                    long nbFeature = (long) result.next().get(QUERY_CYPHER_ALIAS);
                    if (nbFeature == 1) {
                        // This is the last feature of this Group => delete the GROUP
                        graphDb.execute(QUERY_CYPHER_DELETE_GROUP_FEATURE, paramUID );
                    }
                }
            }
            
            // Delete feature
            graphDb.execute(QUERY_CYPHER_DELETE_FEATURE,  paramUID);
            tx.success();
        }
    }

    @Override
    public void update(Feature fp) {
        // Should create Group only if not exist !
        
        
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
