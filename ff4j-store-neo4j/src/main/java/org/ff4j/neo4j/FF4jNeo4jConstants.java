package org.ff4j.neo4j;

/**
 * Cypher requests.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public interface FF4jNeo4jConstants {
    
    /** Cypher query alias. */
    String QUERY_CYPHER_ALIAS = "NB";
    
    // -------------------------------------------------------
    // --------------------- Create  -------------------------  
    // -------------------------------------------------------
    
    /** Cypher query. */
    String QUERY_CYPHER_ADDTO_GROUP = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + "  {uid: {uid} } ), " + 
            "(g:" + FF4jNeo4jLabels.FF4J_FEATURE_GROUP + " {name: {groupName} }) " + 
            "CREATE (f)-[:MEMBER_OF]->(g);";
    
    // -------------------------------------------------------
    // --------------------- Read ----------------------------
    // -------------------------------------------------------
    
    /** Cypher query. */
    String QUERY_CYPHER_EXISTS  = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " { uid:  {uid} }) " + 
            "RETURN count(*) AS " + QUERY_CYPHER_ALIAS;
    
    String QUERY_CYPHER_EXISTS_GROUP =
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE_GROUP + " { name:  {groupName} }) " + 
            "RETURN count(*) AS " + QUERY_CYPHER_ALIAS;
            
    /** Cypher query. */
    String QUERY_CYPHER_READ_FEATURE = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} })--(all) RETURN f,all";
    
    /** Cypher query. */
    String QUERY_CYPHER_NORELATIONSHIPS = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} }) RETURN f;";
    
    /** Cypher query. */
    String QUERY_CYPHER_READ_ALL = 
            "MATCH (f:FF4J_FEATURE)--(all) RETURN f,all;";
    
    /** Cypher query. */
    String QUERY_CYPHER_READ_SINGLE = 
            "MATCH (f:FF4J_FEATURE) RETURN f;";
    
    /** Cypher query. */
    String QUERY_CYPHER_GETGROUPNAME = 
            "MATCH(f:" + FF4jNeo4jLabels.FF4J_FEATURE + "  { uid:  {uid} } ) " + 
             "--(g:" + FF4jNeo4jLabels.FF4J_FEATURE_GROUP + ") " + 
             "RETURN g.name as GROUPNAME;";
    
    /** Cypher query. */
    String QUERY_CYPHER_GET_FLIPPINGSTRATEGY = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} })" + 
            "--(s:" + FF4jNeo4jLabels.FF4J_FLIPPING_STRATEGY + ") " + 
            "RETURN s;";
    
    /** Cypher query. */
    String QUERY_CYPHER_COUNT_FEATURE_OF_GROUP = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " ) " + 
            "WHERE (f)-[:MEMBER_OF]-( { name: {groupName} }) " + 
            "RETURN COUNT(*) AS " + QUERY_CYPHER_ALIAS + ";";
    
    /** Cypher query. */
    String QUERY_CYPHER_READ_FEATURES_OF_GROUP = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " ) " + 
            "WHERE (f)-[:MEMBER_OF]-( { name: {groupName} }) " + 
            "RETURN f.uid AS UID;";
    
    /** Cypher query. */
    String QUERY_READ_GROUPS = 
            "MATCH (g:" +  FF4jNeo4jLabels.FF4J_FEATURE_GROUP + "  ) " + 
            "RETURN g.name AS GROUPNAME;";
    
    // -------------------------------------------------------
    // --------------------- Update  -------------------------  
    // -------------------------------------------------------
    
    /** Cypher query. */
    String QUERY_CYPHER_ENABLE  = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} }) " + 
            "SET f.enable = true RETURN f.enable;";
    
    /** Cypher query. */
    String QUERY_CYPHER_DISABLE = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} }) " + 
            "SET f.enable = false RETURN f.enable;";
    
    /** Cypher query. */
    String QUERY_CYPHER_ADD_ROLE = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + "  {uid: {uid} }) " + 
            "SET f.roles = f.roles + {roleName} return f;";
    
    /** Cypher query. */
    String QUERY_CYPHER_ENABLE_GROUP = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " ) " + 
            "WHERE (f)-[:MEMBER_OF]-( { name: {groupName} }) " + 
            "SET f.enable = true RETURN f.enable;";
    
    /** Cypher query. */
    String QUERY_CYPHER_DISABLE_GROUP = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " ) " + 
            "WHERE (f)-[:MEMBER_OF]-( { name: {groupName} }) " + 
            "SET f.enable = false RETURN f.enable;";
    
  
    
    /** Cypher query. */
    String QUERY_CYPHER_UPDATE_ROLE = 
            "MATCH (f:FF4J_FEATURE { uid: {uid}  }) " + 
            "SET f.roles = {roles} RETURN f";
    
    // -------------------------------------------------------
    // --------------------- Delete --------------------------
    // -------------------------------------------------------
    
    /** Delete properties related to the feature. */
    String QUERY_CYPHER_DELETE_PROPERTIES_FEATURE = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} })--(p:" + FF4jNeo4jLabels.FF4J_PROPERTY + " ) "  + 
            "DETACH DELETE p;";
    
    /** Delete flipping strategy related to the feature. */
    String QUERY_CYPHER_DELETE_STRATEGY_FEATURE = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} })--(s:" + FF4jNeo4jLabels.FF4J_FLIPPING_STRATEGY + ") "  + 
            "DETACH DELETE s;";

    /** Delete flipping strategy related to the feature. */
    String QUERY_CYPHER_DELETE_GROUP_FEATURE = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} })--(s:" + FF4jNeo4jLabels.FF4J_FEATURE_GROUP + ") "  + 
            "DETACH DELETE s;";
    
    /** Delete Feature with all its relationships*/
    String QUERY_CYPHER_DELETE_FEATURE = 
            "MATCH (f:FF4J_FEATURE { uid: {uid}  }) " + 
            "DETACH DELETE f;";
    
    /** Cypher query. */
    String QUERY_CYPHER_REMOVEFROMGROUP = 
            "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: {uid} })-[a:MEMBER_OF]->() DELETE a;";
    
    /** Cypher query. */
    String QUERY_CYPHER_DELETE_GROUP = 
            "MATCH (g:" + FF4jNeo4jLabels.FF4J_FEATURE_GROUP + " { name: {groupName} }) DETACH DELETE g;";
   
    // -------------------------------------------------------
    // --------------------- Attributes  ---------------------  
    // -------------------------------------------------------
    
    /** core attribute. */
    String NODEFEATURE_ATT_UID = "uid";
    
    /** core attribute. */
    String NODEFEATURE_ATT_ENABLE = "enable";
    
    /** core attribute. */
    String NODEFEATURE_ATT_ROLES= "roles";
    
    /** core attribute. */
    String NODEFEATURE_ATT_DESCRIPTION= "description";
    
    /** core attribute. */
    String NODESTRATEGY_ATT_TYPE = "type";
    
    /** core attribute. */
    String NODESTRATEGY_ATT_INITPARAMS = "initParams";
    
    /** core attribute. */
    String NODEPROPERTY_ATT_NAME = "name";
    
    /** core attribute. */
    String NODEPROPERTY_ATT_DESCRIPTION = "description";
    
    /** core attribute. */
    String NODEPROPERTY_ATT_VALUE = "value";
    
    /** core attribute. */
    String NODEPROPERTY_ATT_FIXEDVALUES = "fixedValues";
    
    /** core attribute. */
    String NODEPROPERTY_ATT_TYPE = "type";
    
    /** core attribute. */
    String NODEGROUP_ATT_NAME= "name";
    

}
