package org.ff4j.feature.togglestrategy;

/**
 * The Dark Launch devops pattern is the capacity for a system to enable a new feature for a subset of incoming requests
 * and measure if the new feature introduce some overhead. Without feature toggle you must deploy your new package on a 
 * node of the cluster and measure. With this strategy all nodes of the cluster have the new version a execute new
 * behaviour for a subset of requests. This measure are more realistic and the behaviour should be validated without redeployment.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class DarkLaunchToggleStrategy extends PonderationToggleStrategy {

    /** Serial. */
    private static final long serialVersionUID = -6323214639798455817L;}
