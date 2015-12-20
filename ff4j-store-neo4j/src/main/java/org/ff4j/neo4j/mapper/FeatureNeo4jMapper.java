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
import org.ff4j.neo4j.FF4jNeo4jLabels;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;

/**
 * Map Neo4j node from and to {@link Feature} bean.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FeatureNeo4jMapper {
    
    /** core attribute. */
    public static String ATT_UID = "uid";
    
    /** core attribute. */
    public static String ATT_ENABLE = "enable";
    
    /** core attribute. */
    public static String ATT_ROLES= "roles";
    
    /** core attribute. */
    public static String ATT_DESCRIPTION= "description";
    
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
    static public Node fromFeature2Node(GraphDatabaseService graphDb, Feature feature) {
        Node nodeFeature = graphDb.createNode(FF4jNeo4jLabels.FF4J_FEATURE);
        nodeFeature.setProperty(ATT_UID, feature.getUid());
        nodeFeature.setProperty(ATT_ENABLE, feature.isEnable());
        nodeFeature.setProperty(ATT_DESCRIPTION, feature.getDescription());
        if (feature.getPermissions() != null) {
            nodeFeature.setProperty(ATT_ROLES, feature.getPermissions().toArray(new String[0]));
        }
        return nodeFeature;
    }

}
