package org.ff4j.neo4j.store;

import static org.ff4j.neo4j.FF4jNeo4jConstants.NODEFEATURE_ATT_UID;
import static org.ff4j.neo4j.FF4jNeo4jConstants.NODEGROUP_ATT_NAME;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_ADDTO_GROUP;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_ADD_ROLE;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_ALIAS;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_COUNT_FEATURE_OF_GROUP;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_DELETE_ALLFEATURE;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_DELETE_ALLSINGLEFEATURE;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_DELETE_FEATURE;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_DELETE_GROUP;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_DELETE_GROUP_FEATURE;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_DELETE_PROPERTIES_FEATURE;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_DELETE_STRATEGY_FEATURE;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_DISABLE;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_DISABLE_GROUP;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_ENABLE;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_ENABLE_GROUP;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_EXISTS;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_EXISTS_GROUP;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_GETGROUPNAME;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_GET_FLIPPINGSTRATEGY;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_NORELATIONSHIPS;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_READ_ALL;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_READ_FEATURE;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_READ_FEATURES_OF_GROUP;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_READ_SINGLE;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_REMOVEFROMGROUP;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_CYPHER_UPDATE_ROLE;
import static org.ff4j.neo4j.FF4jNeo4jConstants.QUERY_READ_GROUPS;

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

import static org.ff4j.neo4j.mapper.Neo4jMapper.fromNode2Feature;
import static org.ff4j.neo4j.mapper.Neo4jMapper.fromNode2FlippingStrategy;
import static org.ff4j.neo4j.mapper.Neo4jMapper.fromNode2Property;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.neo4j.FF4jNeo4jLabels;
import org.ff4j.neo4j.FF4jNeo4jRelationShips;
import org.ff4j.property.Property;
import org.ff4j.store.AbstractFeatureStore;
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
public class FeatureStoreNeo4J extends AbstractFeatureStore {

    public static final String GROUPNAME = "GROUPNAME";
    
    public static final String GROUP_NAME = "groupName";
    
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
        assertFeatureExist(uid);
        Transaction tx = graphDb.beginTx();
        Map<String, Object> queryParameters = new HashMap<>();
        queryParameters.put("uid", uid);
        graphDb.execute(QUERY_CYPHER_ENABLE, queryParameters);
        tx.success();
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String uid) {
        assertFeatureExist(uid);
        Transaction tx = graphDb.beginTx();
        Map<String, Object> queryParameters = new HashMap<>();
        queryParameters.put("uid", uid);
        graphDb.execute(QUERY_CYPHER_DISABLE, queryParameters);
        tx.success();
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String featId) {
        assertFeatureExist(featId);
        Feature targetFeature = null;
        Transaction tx = graphDb.beginTx();
        Map<String, Object> queryParameters = new HashMap<>();
        queryParameters.put("uid", featId);
        Result result = graphDb.execute(QUERY_CYPHER_READ_FEATURE, queryParameters);
        if (!result.hasNext()) {
            // Exist but as no relation ship not return by first query
            result = graphDb.execute(QUERY_CYPHER_NORELATIONSHIPS, queryParameters);
        }
        while (result.hasNext()) {
            Map<String, Object> response = result.next();
            if (targetFeature == null) {
                Node nodeFeature = (Node) response.get("f");
                targetFeature = fromNode2Feature(nodeFeature);
            }
            addNeighBour2Feature(targetFeature, (Node) response.get("all"));
        }
        tx.success();
        return targetFeature;
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        Map<String, Feature> allFeatures = new HashMap<>();
        Transaction tx = graphDb.beginTx();
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
        return allFeatures;
    }
    
    /** {@inheritDoc} */
    @Override
    public void delete(String uid) {
        Util.assertHasLength(uid);
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }

        // Parameter
        Map<String, Object> paramUID = new HashMap<>();
        paramUID.put("uid", uid);

        Transaction tx = graphDb.beginTx();

        // Delete Flipping Strategy if it exists
        graphDb.execute(QUERY_CYPHER_DELETE_STRATEGY_FEATURE, paramUID);

        // Delete Related Property if exist
        graphDb.execute(QUERY_CYPHER_DELETE_PROPERTIES_FEATURE, paramUID);

        // Check group
        Result result = graphDb.execute(QUERY_CYPHER_GETGROUPNAME, paramUID);
        if (result.hasNext()) {
            String groupName = (String) result.next().get(GROUPNAME);
            Map<String, Object> paramGroupName = new HashMap<>();
            paramGroupName.put(GROUP_NAME, groupName);
            result = graphDb.execute(QUERY_CYPHER_COUNT_FEATURE_OF_GROUP, paramGroupName);
            if (result.hasNext()) {
                long nbFeature = (long) result.next().get(QUERY_CYPHER_ALIAS);
                if (nbFeature == 1) {
                    // This is the last feature of this Group => delete the GROUP
                    graphDb.execute(QUERY_CYPHER_DELETE_GROUP_FEATURE, paramUID);
                }
            }
        }

        // Delete feature
        graphDb.execute(QUERY_CYPHER_DELETE_FEATURE, paramUID);
        tx.success();
    }
    
    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String uid, String roleName) {
        Util.assertHasLength(roleName);
        Feature feat = read(uid);
        if (feat.getPermissions() != null && !feat.getPermissions().contains(roleName)) {
            Transaction tx = graphDb.beginTx();
            Map<String, Object> paramUID = new HashMap<>();
            paramUID.put("uid", uid);
            paramUID.put("roleName", roleName);
            graphDb.execute(QUERY_CYPHER_ADD_ROLE, paramUID);
            tx.success();
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
            Transaction tx = graphDb.beginTx();
            Map<String, Object> paramUID = new HashMap<>();
            paramUID.put("uid", uid);
            paramUID.put("roles", roles);
            graphDb.execute(QUERY_CYPHER_UPDATE_ROLE, paramUID);
            tx.success();
        }
    }

    /**
     * Create, update or delete Flipping Strategy
     */
    private void updateFlippingStrategy(Feature fp) {
        Transaction tx2 = graphDb.beginTx();
        // Create or update FlippingStrategy (as provided)
        Map<String, Object> queryParameters = new HashMap<>();
        queryParameters.put("uid", fp.getUid());
        if (fp.getFlippingStrategy() != null) {
            Result flippingNode = graphDb.execute(QUERY_CYPHER_GET_FLIPPINGSTRATEGY, queryParameters);
            if (flippingNode.hasNext()) {
                graphDb.execute(createUpdateFlippingStrategy(fp));
            } else {
                graphDb.execute(createQueryFlippingStrategy(fp));
            }
        } else {
            // Delete flipping strategy if exist (as null)
            graphDb.execute(QUERY_CYPHER_DELETE_STRATEGY_FEATURE, queryParameters);
        }
        tx2.success();
    }
    
    /**
     * Update group attribute of feature.
     *
     * @param fp
     *      target feature
     */
    private void updateGroups(Feature fp) {
        // Group
        String oldGroupName = getCurrentGroupName(fp.getUid());
        if ((oldGroupName != null) && (fp.getGroup() == null)) {
            removeFromGroup(fp.getUid(), oldGroupName);
        }
        if (fp.getGroup() != null && fp.getGroup().length() > 0) {
            // group already existed and has been changed
            if (oldGroupName != null && !oldGroupName.equalsIgnoreCase(fp.getGroup())) {
                removeFromGroup(fp.getUid(), oldGroupName);
            }
            // No group before or a new group, different from before
            if (oldGroupName == null || !oldGroupName.equalsIgnoreCase(fp.getGroup())) {
                addToGroup(fp.getUid(), fp.getGroup());
            }
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        Util.assertNotNull(fp);
        Util.assertHasLength(fp.getUid());
        // Check existence
        read(fp.getUid());

        // Create or update core Feature as a first TX
        Transaction tx = graphDb.beginTx();
        graphDb.execute(createUpdateCoreFeature(fp));
        tx.success();
        
        // Create, update or delete Flipping Strategy
        updateFlippingStrategy(fp);

        // Update groups
        updateGroups(fp);

        // Properties
        Transaction tx3 = graphDb.beginTx();
        Map<String, Object> paramUID = new HashMap<>();
        paramUID.put("uid", fp.getUid());
        graphDb.execute(QUERY_CYPHER_DELETE_PROPERTIES_FEATURE, paramUID);
        tx3.success();
        
        if (fp.getCustomProperties() != null && fp.getCustomProperties().size() > 0) {
            for (String pName : fp.getCustomProperties().keySet()) {
                createProperty(fp.getProperty(pName), fp.getUid());
            }
        }
    }
    
    private void createProperty(Property<?> pro, String featureName) {
        StringBuilder cypherCreate = new StringBuilder("MATCH  (f:FF4J_FEATURE {uid:'" + featureName + "'} ) ");
        cypherCreate.append("CREATE (p:" + FF4jNeo4jLabels.FF4J_FEATURE_PROPERTY + " { name :'");
        cypherCreate.append(pro.getName());
        cypherCreate.append("', type:'");
        cypherCreate.append(pro.getType());
        cypherCreate.append("', value:'");
        cypherCreate.append(pro.getValue());
        cypherCreate.append("', fixedValues:[");
        if (pro.getFixedValues() != null && !pro.getFixedValues().isEmpty()) {
            boolean first = true;
            for (Object fix : pro.getFixedValues()) {
                if (!first) {
                    cypherCreate.append(",");
                }
                cypherCreate.append("'" + fix.toString() + "'");
                first = false;
            }
        }
        cypherCreate.append("]");
        if (pro.getDescription() != null && pro.getDescription().length() > 0) {
            cypherCreate.append(", description:'");
            cypherCreate.append(pro.getDescription());
            cypherCreate.append("'");
        }
        cypherCreate.append("})-[:" + FF4jNeo4jRelationShips.PROPERTY_OF + "]->(f);");
        Transaction tx = graphDb.beginTx();
        graphDb.execute(cypherCreate.toString());
        tx.success();
    }

    private String getCurrentGroupName(String uid) {
        String groupName = null;
        Transaction tx = graphDb.beginTx();
        Map<String, Object> queryParameters = new HashMap<>();
        queryParameters.put("uid", uid);
        Result result = graphDb.execute(QUERY_CYPHER_GETGROUPNAME, queryParameters);
        if (result.hasNext()) {
            groupName = (String) result.next().get(GROUPNAME);
        }
        tx.success();
        return groupName;
    }

    private String createQueryFlippingStrategy(Feature fp) {
        String fsCreateQuery = "MATCH  (f:FF4J_FEATURE {uid:'" + fp.getUid() + "'}) ";
        fsCreateQuery += "CREATE (fs:FF4J_FLIPPING_STRATEGY { type:'";
        fsCreateQuery += fp.getFlippingStrategy().getClass().getName();
        fsCreateQuery += "', initParams: [";
        Map < String, String > initParams = fp.getFlippingStrategy().getInitParams();
        if (initParams != null && initParams.size() > 0) {
            boolean first = true;
            for (Map.Entry < String, String > entry : initParams.entrySet()) {
                if (!first) {
                    fsCreateQuery += ",";
                }
                fsCreateQuery += "'" + entry.getKey() + "=" + entry.getValue() + "'";
                first = false;
            }
        }
        fsCreateQuery += "]})-[:" + FF4jNeo4jRelationShips.STRATEGY_OF + "]->(f);";
        return fsCreateQuery;
    }
        
    private String createQueryNewCoreFeature(Feature fp) {
        StringBuilder cypherCreate = new StringBuilder("CREATE (");
        cypherCreate.append("f:" + FF4jNeo4jLabels.FF4J_FEATURE + " { uid :'");
        cypherCreate.append(fp.getUid());
        cypherCreate.append("', enable:");
        cypherCreate.append(fp.isEnable());
        if (fp.getDescription() != null && fp.getDescription().length() > 0) {
            cypherCreate.append(", description:'");
            cypherCreate.append(fp.getDescription());
            cypherCreate.append("'");
        }
        if (fp.getPermissions() != null && fp.getPermissions().size() > 0) {
            cypherCreate.append(", roles: [");
            boolean first = true;
            for(String role : fp.getPermissions()) {
               if (!first) {
                   cypherCreate.append(",");
               }
               cypherCreate.append("'" + role + "'");
               first = false;
            }
            cypherCreate.append("]");
        }
        cypherCreate.append("});");
        return cypherCreate.toString();
    }
    
    private String createUpdateCoreFeature(Feature fp) {
        StringBuilder cypherUpdate = new StringBuilder("MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " { uid:'");
        cypherUpdate.append(fp.getUid());
        cypherUpdate.append("' }) ");
        cypherUpdate.append("SET f.enable = " + fp.isEnable());
        if (fp.getDescription() != null && fp.getDescription().length() > 0) {
            cypherUpdate.append(", f.description = '");
            cypherUpdate.append(fp.getDescription());
            cypherUpdate.append("'");
        }
        if (fp.getPermissions() != null && fp.getPermissions().size() > 0) {
            cypherUpdate.append(", f.roles = [");
            boolean first = true;
            for(String role : fp.getPermissions()) {
               if (!first) {
                   cypherUpdate.append(",");
               }
               cypherUpdate.append("'" + role + "'");
               first = false;
            }
            cypherUpdate.append("]");
        } else {
            // AS role is null
            cypherUpdate.append(", f.roles = []");
        }
        cypherUpdate.append(";");
        return cypherUpdate.toString();
    }
  
    private String createUpdateFlippingStrategy(Feature fp) {
        String queryUpdate = "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: '";
        queryUpdate += fp.getUid() + "' })--(s:FF4J_FLIPPING_STRATEGY) ";
        queryUpdate += "SET s.type='";
        queryUpdate += fp.getFlippingStrategy().getClass().getName();
        queryUpdate += "', s.initParams= [";
        Map < String, String > initParams = fp.getFlippingStrategy().getInitParams();
            if (initParams != null && initParams.size() > 0) {
            boolean first = true;
            for (Map.Entry < String, String > entry : initParams.entrySet()) {
                if (!first) {
                    queryUpdate += ",";
                }
                queryUpdate += "'" + entry.getKey() + "=" + entry.getValue() + "'";
                first = false;
            }
        }
        queryUpdate += "];";
        return queryUpdate;
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
        Transaction tx = graphDb.beginTx();
        // Create core
        graphDb.execute(createQueryNewCoreFeature(fp));

        // Create Flipping Strategy
        if (fp.getFlippingStrategy() != null) {
            graphDb.execute(createQueryFlippingStrategy(fp));
        }

        // Create Group
        if (fp.getGroup() != null && !"".equals(fp.getGroup())) {
            addToGroup(fp.getUid(), fp.getGroup());
        }

        tx.success();
        // Create Properties
        if (fp.getCustomProperties() != null && fp.getCustomProperties().size() > 0) {
            for (String pName : fp.getCustomProperties().keySet()) {
                createProperty(fp.getProperty(pName), fp.getUid());
            }
        }
    }

    // ---------------- GROUPS -------------------

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        Util.assertHasLength(groupName);
        Map < String, Object > queryParameters = new HashMap<>();
        queryParameters.put(GROUP_NAME, groupName);
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
        assertGroupExist(groupName);
        Map<String, Feature> allFeatures = readAll();
        Map<String, Feature> groupFeatures = new HashMap<>();
        Transaction tx = graphDb.beginTx();
        Map<String, Object> paramGroupName = new HashMap<>();
        paramGroupName.put(GROUP_NAME, groupName);
        Result result = graphDb.execute(QUERY_CYPHER_READ_FEATURES_OF_GROUP, paramGroupName);
        while (result.hasNext()) {
            String member = (String) result.next().get("UID");
            groupFeatures.put(member, allFeatures.get(member));
        }
        tx.success();
        return groupFeatures;
    }
    
    /** {@inheritDoc} */
    @Override
    public void enableGroup(String groupName) {
        assertGroupExist(groupName);
        Transaction tx = graphDb.beginTx();
        Map<String, Object> paramGroupName = new HashMap<>();
        paramGroupName.put(GROUP_NAME, groupName);
        graphDb.execute(QUERY_CYPHER_ENABLE_GROUP, paramGroupName);
        tx.success();
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
        assertGroupExist(groupName);
        Transaction tx = graphDb.beginTx();
        Map<String, Object> paramGroupName = new HashMap<>();
        paramGroupName.put(GROUP_NAME, groupName);
        graphDb.execute(QUERY_CYPHER_DISABLE_GROUP, paramGroupName);
        tx.success();
    }
   
    /** {@inheritDoc} */
    @Override
    public void addToGroup(String uid, String groupName) {
        assertFeatureExist(uid);
        Util.assertHasLength(groupName);

        // Create group if not exist
        if (!existGroup(groupName)) {
            Transaction tx = graphDb.beginTx();
            String createGroup = "CREATE (" + groupName + ":" + FF4jNeo4jLabels.FF4J_FEATURE_GROUP;
            createGroup += " { name:'" + groupName + "' });";
            graphDb.execute(createGroup);
            tx.success();
        }

        // Create relation ship (work with indexes)
        Transaction tx = graphDb.beginTx();
        Map<String, Object> params = new HashMap<>();
        params.put("uid", uid);
        params.put(GROUP_NAME, groupName);
        graphDb.execute(QUERY_CYPHER_ADDTO_GROUP, params);
        tx.success();
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String uid, String groupName) {
        assertFeatureExist(uid);
        assertGroupExist(groupName);
        Transaction tx = graphDb.beginTx();
        Map < String, Object > params = new HashMap<>();
        params.put("uid", uid);
        graphDb.execute(QUERY_CYPHER_REMOVEFROMGROUP, params);
        tx.success();
        
        // Delete node group if not more feature on it
        deleteOrphanGroups();
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> readAllGroups() {
        Result result = graphDb.execute(QUERY_READ_GROUPS);
        Set < String > response = new HashSet<>();
        while (result.hasNext()) {
            response.add((String) result.next().get(GROUPNAME));
        }
        return response;
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
        if (nodeOther != null && nodeOther.getLabels().iterator().hasNext()) {
            String nodeLabel = nodeOther.getLabels().iterator().next().toString();
            FF4jNeo4jLabels lblb = FF4jNeo4jLabels.valueOf(nodeLabel);
            switch(lblb) {
                case FF4J_FLIPPING_STRATEGY:
                    FlippingStrategy fs = fromNode2FlippingStrategy(targetFeature.getUid(), nodeOther);
                    targetFeature.setFlippingStrategy(fs);
                break;
                case FF4J_FEATURE_PROPERTY:
                    Property<?> ap = fromNode2Property(nodeOther);
                    targetFeature.getCustomProperties().put(ap.getName(), ap);
                break;
                
                case FF4J_FEATURE_GROUP:
                    targetFeature.setGroup((String) nodeOther.getProperty(NODEGROUP_ATT_NAME));
                break;
                default: break;
            }
        }
    }
    
    /**
     * Delete orphan groups.
     */
    private void deleteOrphanGroups() {
        Set < String > groupNames = readAllGroups();
        for (String groupName : groupNames) {
            Map < String, Object > paramGroupName = new HashMap<>();
            paramGroupName.put(GROUP_NAME, groupName);
            Result result = graphDb.execute(QUERY_CYPHER_COUNT_FEATURE_OF_GROUP, paramGroupName);
            if (result.hasNext()) {
                long nbFeature = (long) result.next().get(QUERY_CYPHER_ALIAS);
                if (nbFeature == 0) {
                    // This is the last feature of this Group => delete the GROUP
                    graphDb.execute(QUERY_CYPHER_DELETE_GROUP, paramGroupName );
                }
            }
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public void clear() {
        Transaction tx = graphDb.beginTx();
        graphDb.execute(QUERY_CYPHER_DELETE_ALLFEATURE);
        graphDb.execute(QUERY_CYPHER_DELETE_ALLSINGLEFEATURE);
        tx.success();
    }
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        try (Transaction tx = graphDb.beginTx() ) {
            if (!graphDb.schema().getConstraints(FF4jNeo4jLabels.FF4J_FEATURE).iterator().hasNext()) {
                graphDb.schema().constraintFor(FF4jNeo4jLabels.FF4J_FEATURE)//
                        .assertPropertyIsUnique(NODEFEATURE_ATT_UID)//
                        .create();
            }
            if (!graphDb.schema().getConstraints(FF4jNeo4jLabels.FF4J_FEATURE_GROUP).iterator().hasNext()) {
                graphDb.schema().constraintFor(FF4jNeo4jLabels.FF4J_FEATURE_GROUP)//
                    .assertPropertyIsUnique(NODEGROUP_ATT_NAME )//
                    .create();
            }
            tx.success();
        }
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
