package org.ff4j.consul;

/**
 * Set of keys to register ff4j into Consul registry.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class ConsulConstants {

    /** Global Key. */
    public static final String FF4J_KEY_FF4J = "FF4J";
    
    /** Path for ff4j keys. */
    public static final String FF4J_PREFIXKEY_FEATURES = FF4J_KEY_FF4J + "/FEATURES/";
    
    /** Path for ff4j keys. */
    public static final String FF4J_PREFIXKEY_PROPERTIES = FF4J_KEY_FF4J + "/PROPERTIES/";
    
    /** Path for ff4j keys. */
    public static final String FF4J_PREFIXKEY_AUDIT = FF4J_KEY_FF4J + "/AUDIT/";
    
    /** Path for ff4j keys. */
    public static final String FF4J_PREFIXKEY_HITS = FF4J_KEY_FF4J + "/HITS/";
    
    /** Path for ff4j keys. */
    public static final String FF4J_PREFIXKEY_MISS = FF4J_KEY_FF4J + "/MISS/";
}
