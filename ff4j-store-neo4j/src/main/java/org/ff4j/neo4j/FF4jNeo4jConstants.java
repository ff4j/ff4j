package org.ff4j.neo4j;

public interface FF4jNeo4jConstants {
    
    String QUERY_CYPHER_EXISTS = "MATCH (f:" + FF4jNeo4jLabels.FF4J_FEATURE + " { uid: '?' }) RETURN count(*);";

}
