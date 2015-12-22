package org.ff4j.neo4j.mapper;

import java.util.Arrays;
import java.util.HashSet;

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

import org.ff4j.core.Feature;
import org.ff4j.neo4j.FF4jNeo4jConstants;
import org.ff4j.neo4j.FF4jNeo4jLabels;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;

/**
 * Map Neo4j node from and to {@link Feature} bean.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FeatureNeo4jMapper implements FF4jNeo4jConstants {
    
    /**
     * Hide default constructor.
     */
    private FeatureNeo4jMapper() {
    }
    
    /**
     * Transform  node FF4J_FEATURE into core Feature.
     *
     * @param nodeFeature
     * @return
     */
    public static Feature fromNode2Feature(Node nodeFeature) {
        String featureID    = (String) nodeFeature.getProperty(NODEFEATURE_ATT_UID);
        Boolean enable      = (Boolean) nodeFeature.getProperty(NODEFEATURE_ATT_ENABLE);
        String description  = (String) nodeFeature.getProperty(NODEFEATURE_ATT_DESCRIPTION);
        String[] roles      = (String[]) nodeFeature.getProperty(NODEFEATURE_ATT_ROLES);
        Feature feature = new Feature(featureID, enable);
        if (description != null) {
            feature.setDescription(description);
        }
        if (roles != null) {
            feature.setPermissions(new HashSet<>(Arrays.asList(roles)));
        }
        return feature;
    }
    
    /**
     * Create Node from Feature.
     *
     * @param feature
     *      target feature
     * @return
     *      target node
     */
    static public Node fromFeature2Node(GraphDatabaseService graphDb, Feature feature) {
        Node nodeFeature = graphDb.createNode(FF4jNeo4jLabels.FF4J_FEATURE);
        nodeFeature.setProperty(NODEFEATURE_ATT_UID, feature.getUid());
        nodeFeature.setProperty(NODEFEATURE_ATT_ENABLE, feature.isEnable());
        nodeFeature.setProperty(NODEFEATURE_ATT_DESCRIPTION, feature.getDescription());
        if (feature.getPermissions() != null) {
            nodeFeature.setProperty(NODEFEATURE_ATT_ROLES, feature.getPermissions().toArray(new String[0]));
        }
        return nodeFeature;
    }
    
    

}
