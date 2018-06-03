package org.ff4j.security.domain;

/**
 * Single Table of permission, ;ultiple targets.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public enum FF4jGranteeType {
    
    /** Higher level **/
    FF4J,
    
    /** Permission is related to a Feature. */
    FEATURE,
    
    /** Permission is related to a Property. */
    PROPERTY,
    
    /** Permisison is part of a role. */
    ROLE,
    
    /** Feature Store. */
    FEATURE_STORE,
    
    /** Feature Store. */
    PROPERTY_STORE,
    
    /** Web console if relevant. */
    WEB_UI,
    
    /** CRUD operations. */
    REST_API,
    
    /** JMX CRUD operations. */
    JMX_API,
    
    /** Let people add their own if relevant for them. */
    CUSTOM;

}
