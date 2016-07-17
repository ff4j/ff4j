package org.ff4j.cassandra;

/**
 * Constants to work with 
 *
 * @author Cedrick Lunven (@clunven)
 */
public class CassandraConstants {
    
    /** Keyspace is equivalent to Database in Cassandra. */
    public static final String KEYSPACE = "FF4J";
    
    /** Default cassandra parameter. */
    public static final int PORT = 7000;
    
    /** Default cassandra parameter. */
    public static final int PORT_TLS = 7001;
    
    /** Default cassandra parameter. */
    public static final int PORT_JMX = 7199;
    
    /** Default cassandra parameter. */
    public static final int PORT_THRIFT = 9160 ;
    
    /** Default cassandra parameter. */
    public static final int PORT_CQL_NATIVE = 9042;
    
    /** Default cassandra parameter. */
    public static final String ADMIN_LOGIN = "cassandra";
    
    /** Default cassandra parameter. */
    public static final String ADMIN_PWD = "cassandra";
    
    /** Default cassandra parameter. */
    public static final String DEFAULT_HOST = "127.0.0.1";
    
    /** CQL to work with features. */
    public static final String CQL_EXIST_FEATURE = "SELECT COUNT(*) AS NB FROM ff4j.features WHERE FEAT_UID = ?";
    
    
    /** Remove public constructor. */
    private CassandraConstants() {
    }

}
