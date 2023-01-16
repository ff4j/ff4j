package org.ff4j.loadbalancer;

/**
 * Load balancing strategy
 */
public enum LoadBalancingPolicy {

    /** Using weights. **/
    WEIGHT_LOAD_BALANCING,

    /** Load balancing **/
    ROUND_ROBIN,
    
    /** Pick a resource randomly. */
    RANDOM
}
