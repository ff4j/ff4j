package org.ff4j.neo4j;

/**
 * Cypher requests.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public interface FF4jNeo4jConstants {
    
    /** Cypher query alias. */
    String QUERY_CYPHER_ALIAS = "NB";
    
    /** Cypher query. */
    String QUERY_CYPHER_EXISTS  = "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: '%s' }) RETURN count(*) AS " + QUERY_CYPHER_ALIAS;
    
    /** Cypher query. */
    String QUERY_CYPHER_ENABLE  = "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: '%s' }) SET f.enable = true RETURN f.enable;";
    
    /** Cypher query. */
    String QUERY_CYPHER_DISABLE = "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: '%s' }) SET f.enable = false RETURN f.enable;";
    
    /** Cypher query. */
    String QUERY_CYPHER_READ_FEATURE = "MATCH (f:FF4J_FEATURE { uid: {uid} })--(all) RETURN f,all";
    
    /** core attribute. */
    String NODEFEATURE_ATT_UID = "uid";
    
    /** core attribute. */
    String NODEFEATURE_ATT_ENABLE = "enable";
    
    /** core attribute. */
    String NODEFEATURE_ATT_ROLES= "roles";
    
    /** core attribute. */
    String NODEFEATURE_ATT_DESCRIPTION= "description";
    
    /** core attribute. */
    String NODEGROUP_ATT_NAME= "name";

}
