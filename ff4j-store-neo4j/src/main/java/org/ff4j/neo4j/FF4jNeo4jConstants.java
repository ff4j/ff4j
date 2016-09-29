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


/**
 * Cypher requests.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FF4jNeo4jConstants {
    
    /** Cypher query alias. */
    public static final String QUERY_CYPHER_ALIAS = "NB";
    
    // -------------------------------------------------------
    // --------------------- Attributes  ---------------------  
    // -------------------------------------------------------
    
    /** core attribute. */
    public static final String NODEFEATURE_ATT_UID = "uid";
    
    /** core attribute. */
    public static final String NODEFEATURE_ATT_ENABLE = "enable";
    
    /** core attribute. */
    public static final String NODEFEATURE_ATT_ROLES= "roles";
    
    /** core attribute. */
    public static final String NODEFEATURE_ATT_DESCRIPTION= "description";
    
    /** core attribute. */
    public static final String NODESTRATEGY_ATT_TYPE = "type";
    
    /** core attribute. */
    public static final String NODESTRATEGY_ATT_INITPARAMS = "initParams";
    
    /** core attribute. */
    public static final String NODEPROPERTY_ATT_NAME = "name";
    
    /** core attribute. */
    public static final String NODEPROPERTY_ATT_DESCRIPTION = "description";
    
    /** core attribute. */
    public static final String NODEPROPERTY_ATT_VALUE = "value";
    
    /** core attribute. */
    public static final String NODEPROPERTY_ATT_FIXEDVALUES = "fixedValues";
    
    /** core attribute. */
    public static final String NODEPROPERTY_ATT_TYPE = "type";
    
    /** core attribute. */
    public static final String NODEGROUP_ATT_NAME= "name";
    
    // -------------------------------------------------------
    // --------------------- Create  -------------------------  
    // -------------------------------------------------------

    public static final String MATCH_F = "MATCH (f:";
    /** Cypher query. */
    public static final String QUERY_CYPHER_ADDTO_GROUP =
            MATCH_F + FF4jNeo4jLabels.FF4J_FEATURE + "  {uid: {uid} } ), " +
            "(g:" + FF4jNeo4jLabels.FF4J_FEATURE_GROUP + " {name: {groupName} }) " + 
            "CREATE (f)-[:" + FF4jNeo4jRelationShips.MEMBER_OF + "]->(g);";
    
    // -------------------------------------------------------
    // --------------------- Read ----------------------------
    // -------------------------------------------------------

    public static final String RETURN_COUNT_AS = "RETURN count(*) AS ";
    /** Cypher query. */
    public static final String QUERY_CYPHER_EXISTS  =
            MATCH_F + FF4jNeo4jLabels.FF4J_FEATURE + " { uid:  {uid} }) " +
                    RETURN_COUNT_AS + QUERY_CYPHER_ALIAS;

    public static final String MATCH_P = "MATCH (p:";
    public static final String QUERY_CYPHER_EXISTS_PROPERTY  =
            MATCH_P + FF4jNeo4jLabels.FF4J_PROPERTY + " { name:  {name} }) " +
                    RETURN_COUNT_AS + QUERY_CYPHER_ALIAS;

    public static final String QUERY_CYPHER_EXISTS_GROUP =
            MATCH_F + FF4jNeo4jLabels.FF4J_FEATURE_GROUP + " { name:  {groupName} }) " +
                    RETURN_COUNT_AS + QUERY_CYPHER_ALIAS;
            
    /** Cypher query. */
    public static final String QUERY_CYPHER_READ_FEATURE =
            MATCH_F + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} })--(all) RETURN f,all";
    
    /** Cypher query. */
    public static final String QUERY_CYPHER_READ_PROPERTY =
            MATCH_P + FF4jNeo4jLabels.FF4J_PROPERTY + " { name: {name} }) RETURN p";
    
    /** Cypher query. */
    public static final String QUERY_CYPHER_NORELATIONSHIPS =
            MATCH_F + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} }) RETURN f;";
    
    /** Cypher query. */
    public static final String QUERY_CYPHER_READ_ALL =
            MATCH_F + FF4jNeo4jLabels.FF4J_FEATURE + ")--(all) RETURN f,all;";
    
    /** Cypher query. */
    public static final String QUERY_CYPHER_READ_ALLPROPERTIES =
            MATCH_P + FF4jNeo4jLabels.FF4J_PROPERTY + ") RETURN p;";
    
    /** Cypher query. */
    public static final String QUERY_CYPHER_READ_SINGLE =
            "MATCH (f:FF4J_FEATURE) RETURN f;";
    
    /** Cypher query. */
    public static final String QUERY_CYPHER_GETGROUPNAME =
            "MATCH(f:" + FF4jNeo4jLabels.FF4J_FEATURE + "  { uid:  {uid} } ) " + 
             "--(g:" + FF4jNeo4jLabels.FF4J_FEATURE_GROUP + ") " + 
             "RETURN g.name as GROUPNAME;";
    
    /** Cypher query. */
    public static final String QUERY_CYPHER_GET_FLIPPINGSTRATEGY =
            MATCH_F + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} })" +
            "--(s:" + FF4jNeo4jLabels.FF4J_FLIPPING_STRATEGY + ") " + 
            "RETURN s;";

    public static final String NAME_GROUP_NAME = "]-( { name: {groupName} }) ";
    public static final String WHERE_F = "WHERE (f)-[:";
    /** Cypher query. */
    public static final String QUERY_CYPHER_COUNT_FEATURE_OF_GROUP =
            MATCH_F + FF4jNeo4jLabels.FF4J_FEATURE + " ) " +
                    WHERE_F + FF4jNeo4jRelationShips.MEMBER_OF + NAME_GROUP_NAME +
            "RETURN COUNT(*) AS " + QUERY_CYPHER_ALIAS + ";";
    
    /** Cypher query. */
    public static final String QUERY_CYPHER_READ_FEATURES_OF_GROUP =
            MATCH_F + FF4jNeo4jLabels.FF4J_FEATURE + " ) " +
                    WHERE_F + FF4jNeo4jRelationShips.MEMBER_OF + NAME_GROUP_NAME +
            "RETURN f.uid AS UID;";
    
    /** Cypher query. */
    public static final String QUERY_READ_GROUPS =
            "MATCH (g:" +  FF4jNeo4jLabels.FF4J_FEATURE_GROUP + "  ) " + 
            "RETURN g.name AS GROUPNAME;";
    
    /** Cypher query. */
    public static final String QUERY_READ_PROPERTYNAMES =
            MATCH_P +  FF4jNeo4jLabels.FF4J_PROPERTY + "  ) " +
            "RETURN p.name AS NAME;";
    
    // -------------------------------------------------------
    // --------------------- Update  -------------------------  
    // -------------------------------------------------------
    
    /** Cypher query. */
    public static final String QUERY_CYPHER_ENABLE  =
            MATCH_F + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} }) " +
            "SET f.enable = true RETURN f.enable;";
    
    
    /** Cypher query. */
    public static final String QUERY_CYPHER_UPDATE_PROPERTYVALUE  =
            MATCH_P + FF4jNeo4jLabels.FF4J_PROPERTY + " { name: {name} }) " +
            "SET p." + NODEPROPERTY_ATT_VALUE + "= {value};";
    
    /** Cypher query. */
    public static final String QUERY_CYPHER_DISABLE =
            MATCH_F + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} }) " +
            "SET f.enable = false RETURN f.enable;";
    
    /** Cypher query. */
    public static final String QUERY_CYPHER_ADD_ROLE =
            MATCH_F + FF4jNeo4jLabels.FF4J_FEATURE + "  {uid: {uid} }) " +
            "SET f.roles = f.roles + {roleName} return f;";
    
    /** Cypher query. */
    public static final String QUERY_CYPHER_ENABLE_GROUP =
            MATCH_F + FF4jNeo4jLabels.FF4J_FEATURE + " ) " +
                    WHERE_F + FF4jNeo4jRelationShips.MEMBER_OF + NAME_GROUP_NAME +
            "SET f.enable = true RETURN f.enable;";
    
    /** Cypher query. */
    public static final String QUERY_CYPHER_DISABLE_GROUP =
            MATCH_F + FF4jNeo4jLabels.FF4J_FEATURE + " ) " +
                    WHERE_F + FF4jNeo4jRelationShips.MEMBER_OF + NAME_GROUP_NAME +
            "SET f.enable = false RETURN f.enable;";
    
    /** Cypher query. */
    public static final String QUERY_CYPHER_UPDATE_ROLE =
            "MATCH (f:FF4J_FEATURE { uid: {uid}  }) " + 
            "SET f.roles = {roles} RETURN f";
    
    // -------------------------------------------------------
    // --------------------- Delete --------------------------
    // -------------------------------------------------------
    
    /** Delete properties related to the feature. */
    public static final String QUERY_CYPHER_DELETE_PROPERTIES_FEATURE =
            MATCH_F + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} })--(p:" + FF4jNeo4jLabels.FF4J_FEATURE_PROPERTY + " ) "  +
            "DETACH DELETE p;";
    
    /** Delete flipping strategy related to the feature. */
    public static final String QUERY_CYPHER_DELETE_STRATEGY_FEATURE =
            MATCH_F + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} })--(s:" + FF4jNeo4jLabels.FF4J_FLIPPING_STRATEGY + ") "  +
            "DETACH DELETE s;";

    /** Delete flipping strategy related to the feature. */
    public static final String QUERY_CYPHER_DELETE_GROUP_FEATURE =
            MATCH_F + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} })--(s:" + FF4jNeo4jLabels.FF4J_FEATURE_GROUP + ") "  +
            "DETACH DELETE s;";
    
    /** Delete Feature with all its relationships*/
    public static final String QUERY_CYPHER_DELETE_FEATURE =
            MATCH_F + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid}  }) " +
            "DETACH DELETE f;";
    
    /** Delete property. */
    public static final String QUERY_CYPHER_DELETE_PROPERTY =
            MATCH_P + FF4jNeo4jLabels.FF4J_PROPERTY + " { name: {name}  }) " +
                    "DETACH DELETE p;";
    
    /** Cypher query. */
    public static final String QUERY_CYPHER_REMOVEFROMGROUP =
            MATCH_F + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} })-[a:" + FF4jNeo4jRelationShips.MEMBER_OF + "]->() DELETE a;";
    
    /** Cypher query. */
    public static final String QUERY_CYPHER_DELETE_GROUP =
            "MATCH (g:" + FF4jNeo4jLabels.FF4J_FEATURE_GROUP + " { name: {groupName} }) DETACH DELETE g;";
    
    /** Cypher query. */
    public static final String QUERY_CYPHER_DELETE_ALLFEATURE =
            "MATCH (f:FF4J_FEATURE)--(all) DETACH DELETE f, all;";
    
    /** Cypher query. */
    public static final String QUERY_CYPHER_DELETE_ALLSINGLEFEATURE =
            MATCH_F + FF4jNeo4jLabels.FF4J_FEATURE + ") DETACH DELETE f;";
    
    /** Cypher query. */
    public static final String QUERY_CYPHER_DELETE_ALLPROPERTY =
            MATCH_P + FF4jNeo4jLabels.FF4J_PROPERTY + ") DETACH DELETE p;";
    
    /**
     * Hide default constructor.
     */
    private FF4jNeo4jConstants() {}
}
