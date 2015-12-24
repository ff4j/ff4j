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
import java.util.HashSet;
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
        assertUID(uid);
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
        assertUID(uid);
        try (Transaction tx = graphDb.beginTx()) {
            Map < String, Object > queryParameters = new HashMap<>();
            queryParameters.put("uid", uid);
            graphDb.execute(QUERY_CYPHER_DISABLE,  queryParameters);
            tx.success();
        }
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String featId) {
        assertUID(featId);
        Feature targetFeature = null;
        try (Transaction tx = graphDb.beginTx()) {
            // Build Query
            Map < String, Object > queryParameters = new HashMap<>();
            queryParameters.put("uid", featId);
            Result result = graphDb.execute(QUERY_CYPHER_READ_FEATURE,  queryParameters);
            if (!result.hasNext()) {
                // Exist but as no relation ship not return by first query
                result = graphDb.execute(QUERY_CYPHER_NORELATIONSHIPS,  queryParameters);
            }
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
    
    /**
     * Validate feature uid.
     *
     * @param uid
     *      target uid
     */
    private void assertUID(String uid) {
        Util.assertHasLength(uid);
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        Map<String, Feature> allFeatures = new HashMap<>();
        try (Transaction tx = graphDb.beginTx()) {
            // Node with relationships
            Result result = graphDb.execute(QUERY_CYPHER_READ_ALL);
            while (result.hasNext()) {
                addToFeatureList(result.next(), allFeatures);
            }
            // Single nodes
            result = graphDb.execute(QUERY_CYPHER_READ_SINGLE);
            while (result.hasNext()) {
                addToFeatureList(result.next(), allFeatures);
            }
           
            tx.success();
        }          
        return allFeatures;
    }
    
    /**
     * Common behaviour to add a result to the feature list.
     * 
     * @param response
     *      target response
     * @param allFeatures
     *      target feature
     */
    private void addToFeatureList(Map < String, Object > response,  Map<String, Feature> allFeatures) {
        Node nodeFeature = (Node) response.get("f");
        String uid = (String) nodeFeature.getProperty(NODEFEATURE_ATT_UID);
        // first time meeting this uid, create core Feature object from 'f'
        if (!allFeatures.containsKey(uid)) {
            Feature currentFeature = fromNode2Feature(nodeFeature);
            allFeatures.put(currentFeature.getUid(), currentFeature);
        }
        if (null != response.get("all")) {
            addNeighBour2Feature(allFeatures.get(uid), (Node) response.get("all"));
        }
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
    
    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String uid, String roleName) {
        Util.assertHasLength(roleName);
        Feature feat = read(uid);
        if (feat.getPermissions() != null && !feat.getPermissions().contains(roleName)) {
            try (Transaction tx = graphDb.beginTx()) {
                Map < String, Object > paramUID = new HashMap<>();
                paramUID.put("uid", uid);
                paramUID.put("roleName", roleName);
                graphDb.execute(QUERY_CYPHER_ADD_ROLE, paramUID);
                tx.success();
            }
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String uid, String roleName) {
        Util.assertHasLength(roleName);
        Feature feat = read(uid);
        if (feat.getPermissions() != null && feat.getPermissions().contains(roleName)) {
            feat.getPermissions().remove(roleName);
            String[] roles = feat.getPermissions().toArray(new String[0]);
            try (Transaction tx = graphDb.beginTx()) {
                Map < String, Object > paramUID = new HashMap<>();
                paramUID.put("uid", uid);
                paramUID.put("roles", roles);
                graphDb.execute(QUERY_CYPHER_REMOVE_ROLE, paramUID);
                tx.success();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        // Update CORE description, enable, roles
        
        // Should create Group only if not exist
        // Should create properties if not exist
        // Should create FlippingStrategy if not exist
        // Should remove flipping stratgy if relevant
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
       
    }
    
    // ---------------- GROUPS -------------------

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        Util.assertHasLength(groupName);
        Map < String, Object > queryParameters = new HashMap<>();
        queryParameters.put("groupName", groupName);
        Result result = graphDb.execute(QUERY_CYPHER_EXISTS_GROUP,  queryParameters);
        Object count = null;
        if (result.hasNext()) {
            count = result.next().get(QUERY_CYPHER_ALIAS);
        }
        return (null != count) && (((long) count) > 0);
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        Util.assertHasLength(groupName);
        Map<String, Feature> allFeatures   = readAll();
        Map<String, Feature> groupFeatures = new HashMap<>();
        try (Transaction tx = graphDb.beginTx()) {
            Map < String, Object > paramGroupName = new HashMap<>();
            paramGroupName.put("groupName", groupName);
            Result result = graphDb.execute(QUERY_CYPHER_READ_FEATURES_OF_GROUP, paramGroupName);
            while (result.hasNext()) {
                String member = (String) result.next().get("UID");
                groupFeatures.put(member, allFeatures.get(member));
            }
            tx.success();
        }
        return groupFeatures;
    }

    
    /** {@inheritDoc} */
    @Override
    public void enableGroup(String groupName) {
        Util.assertHasLength(groupName);
        try (Transaction tx = graphDb.beginTx()) {
            Map < String, Object > paramGroupName = new HashMap<>();
            paramGroupName.put("groupName", groupName);
            graphDb.execute(QUERY_CYPHER_ENABLE_GROUP, paramGroupName);
            tx.success();
        }
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
        Util.assertHasLength(groupName);
        try (Transaction tx = graphDb.beginTx()) {
            Map < String, Object > paramGroupName = new HashMap<>();
            paramGroupName.put("groupName", groupName);
            graphDb.execute(QUERY_CYPHER_DISABLE_GROUP, paramGroupName);
            tx.success();
        }
    }   

   
    /** {@inheritDoc} */
    @Override
    public void addToGroup(String uid, String groupName) {
        assertUID(uid);
        Util.assertHasLength(groupName);
        
        // Create group if not exist
        
        // Create relation ship (work with indexes)
        
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String uid, String groupName) {
        assertUID(uid);
        Util.assertHasLength(groupName);
        // Break Relationship
        // Delete node if no more feature on it
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> readAllGroups() {
        Result result = graphDb.execute(QUERY_READ_GROUPS);
        Set < String > response = new HashSet<>();
        while (result.hasNext()) {
            response.add((String) result.next().get("GROUPNAME"));
        }
        return response;
    }
    
    // ---------------------- Only for cache --------------

    /** {@inheritDoc} */
    @Override
    public boolean isCached() {
        return false;
    }

    /** {@inheritDoc} */
    @Override
    public String getCacheProvider() {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public String getCachedTargetStore() {
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
