package org.ff4j.neo4j.mapper;

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

public class FeatureNeo4jMapper implements FF4jNeo4jConstants {
    
    /**
     * Hide default constructor.
     */
    private FeatureNeo4jMapper() {
    }
    
    /**
     * Create Node from Feature.
     *
     * @param feature
     *      target feature
     * @return
     *      target node
     */
    static public Node getFeatureNode(GraphDatabaseService graphDb, Feature feature) {
        Node nodeFeature = graphDb.createNode(FF4jNeo4jLabels.FEATURE);
        nodeFeature.setProperty(P_FEATURE_UID, feature.getUid());
        nodeFeature.setProperty(P_FEATURE_DESCRIPTION, feature.getDescription());
        nodeFeature.setProperty(P_FEATURE_ENABLE, feature.isEnable());
        return nodeFeature;
    }

}
