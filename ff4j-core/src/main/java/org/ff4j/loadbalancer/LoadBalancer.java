package org.ff4j.loadbalancer;

import org.ff4j.loadbalancer.exception.NoneResourceAvailableException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

/**
 * Generic implementation of a client-side load balancer. It will handle
 * multiple algorithms: RANDOM, LOAD BALANCING, WEIGHT BALANCING.
 * 
 * @param <RSC>
 *     resource, here backend
 */
public class LoadBalancer< RSC >  {
    
    /** Logger for our Client. */
    private static final Logger LOGGER = LoggerFactory.getLogger(LoadBalancer.class);
   
    /** Constants to compute percentage. **/
    private static final double HUNDRED = 100.0;

    /** Constants for millis. **/
    private static final long THOUSAND   = 1000;

    /** Invocation counts since last failure. **/
    private double totalCount = 0;

    /** Total number of Api call(s). **/
    private double globalCount = 0;

    /** How many resources are currently available. **/
    private int unavailableCount = 0;

    /** How much time a component should stay unavailable before another evaluation. **/
    private int unavailabilityPeriod = 10;

    /** Policy used. **/
    private final LoadBalancingPolicy loadBalancingPolicy;

    /** List of resources to load balance. **/
    private List <LoadBalancedResource< RSC >> resources = new ArrayList <>();
    
    /**
     * Constructor with default policy LOAD BALANCING.
     *
     * @param lst
     *      list of resources
     */
    @SuppressWarnings("unchecked")
    public LoadBalancer(RSC... lst) {
        this(LoadBalancingPolicy.ROUND_ROBIN, lst);
    }
    
    /**
     * Initialization of the resources. Weights are part of {@link LoadBalancedResource}.
     *
     * @param policy
     *          current policy
     * @param listRsc
     *          list of resources
     */
    public LoadBalancer(LoadBalancingPolicy policy, List<LoadBalancedResource< RSC >> listRsc) {
        this.loadBalancingPolicy = policy;
        this.resources  = listRsc;
        Collections.sort(this.resources);
    }
    
    /**
     * Initialization of the resources. Weights are part of {@link LoadBalancedResource}.
     *
     * @param policy
     *          current policy
     * @param resources
     *          list of resources
     */
    @SuppressWarnings("unchecked")
    public LoadBalancer(LoadBalancingPolicy policy, RSC... resources) {
        this.loadBalancingPolicy = policy;
        for (RSC rsc : resources) {
            LoadBalancedResource< RSC > lbRsc = new LoadBalancedResource<>(rsc);
            lbRsc.setAvailable(true);
            lbRsc.setNbUse(0);
            // Set coefficient all equals
            if (loadBalancingPolicy == LoadBalancingPolicy.ROUND_ROBIN) {
                lbRsc.setDefaultWeight(HUNDRED / resources.length);
            }
            lbRsc.setCurrentWeight(lbRsc.getDefaultWeight());
            this.resources.add(lbRsc);
        }
        // unavailable first, check unavailability time and put i it back in the pool if available again.
        Collections.sort(this.resources);
    }
    
    /**
     * Main method to retrieve a resource from load-balancing.
     *
     * @return
     *      current resource
     */
    public final synchronized LoadBalancedResource< RSC > getLoadBalancedResource() {
        totalCount++;
        globalCount++;
        switch(loadBalancingPolicy) {
            case WEIGHT_LOAD_BALANCING:
            case ROUND_ROBIN:
                if (unavailableCount == resources.size()) {
                    throw new NoneResourceAvailableException("Cannot retrieve a resource "
                            + "all '" + unavailableCount + "' resources are down.");
                }
                for (LoadBalancedResource< RSC > rsc : resources) {
                    // if resource need to be reintroduced 
                    if (shouldEnableResource(rsc)) {
                        rsc.setAvailable(true);
                        LOGGER.info("{} has reached ends of its unavailability period, putting it back in the pool", rsc.getId());
                        redistributeWeights();
                        return getLoadBalancedResource();
                    }
                    // the resource did not reach its limits
                    if ((HUNDRED * (rsc.getNbUse() / totalCount)) <= rsc.getCurrentWeight()) {
                        rsc.setNbUse(rsc.getNbUse() + 1);
                        return rsc;
                    }
                }
            break;
            case RANDOM:
                return resources.get(new java.util.Random().nextInt(resources.size()));
        }
        throw new NoneResourceAvailableException("Cannot retrieve a resource "
                + "with round robin weights all consumed or unavailable");
    }
    
    /**
     * Main method, provide an available resource.
     * 
     * @return
     *      resource name
     */
    public RSC get() {
        return getLoadBalancedResource().getResource();
    }
    
    /**
     * Recompute weight when one is unavailable.
     */
    private void redistributeWeights() {
        double loadToBalance = 0.0;
        totalCount       = 0;
        unavailableCount = 0;
        
        // Compute load distribution
        for (LoadBalancedResource< RSC > rsc : resources) {
            rsc.setNbUse(0);
            // Resource is NOT available the load need to be redistributed
            if (!rsc.isAvailable()) {
                unavailableCount++;
                loadToBalance += rsc.getDefaultWeight();
            }
        }
        // Load to be redistributed equally among remaining nodes (and NOT reapply proportions)
        double loadToDistribute = loadToBalance / (double) (resources.size() - unavailableCount);
        // Add the load
        for (LoadBalancedResource< RSC > wrapper2 : resources) {
            if (wrapper2.isAvailable()) {
                wrapper2.setCurrentWeight(wrapper2.getDefaultWeight() + loadToDistribute);
            } else {
                wrapper2.setCurrentWeight(0);
            }
            
        }
        // Sorting with unavailable first to be tested.
        Collections.sort(resources);
        LOGGER.info("Resources status after weight computation:");
        for (LoadBalancedResource< RSC > w : resources) {
            LOGGER.info(" + " + w.getId() + ": " + w.getCurrentWeight() );
        }
        
    }

    /**
     * Test unavailable resource.
     * 
     * @param rsc
     *      current resources
     * @return
     *      if 
     */
    private boolean shouldEnableResource(LoadBalancedResource< RSC > rsc) {
        return !rsc.isAvailable() && 
                (System.currentTimeMillis() - rsc.getUnavailabilityTriggerDate().getTime()) > 
                  (THOUSAND * unavailabilityPeriod);
    }
    
    /** {@inheritDoc} **/
    @Override
    public final String toString() {
        StringBuilder strBuildDer = new StringBuilder();
        strBuildDer.append("\nLoadBalanced state : globalCount <")
                   .append(globalCount)
                   .append("> totalCount <")
                   .append(totalCount).append("> ");
        strBuildDer.append(" unavailableCount <")
                   .append(unavailableCount).append(">");
        for (LoadBalancedResource< RSC > wrapper : resources) {
            strBuildDer.append("\n").append(wrapper.toString());
            if (wrapper.isAvailable()) {
                strBuildDer.append(" currentUse ")
                           .append(Double.valueOf(HUNDRED * (wrapper.getNbUse() / totalCount)).intValue())
                           .append("%");
            }
        }
        return strBuildDer.toString();
    }

    /**
     * Handle error. Make resources unavailable.
     * Redistribute the load on remaining resources, and reset counts
     *
     * @param component
     *          component in error
     * @param parentException
     *         parent exception
     * @return
     *        load-balancer
     */
    public final LoadBalancedResource< RSC > handleComponentError(
            final LoadBalancedResource< RSC > component,
            final Throwable parentException) {
        component.setAvailable(false);
        component.setUnavailabilityCause(parentException.getMessage());
        component.setUnavailabilityError(parentException);
        component.setUnavailabilityTriggerDate(new Date());
        redistributeWeights();
        
        return getLoadBalancedResource();
    }

    /**
     * Access resource list
     *
     * @return
     *      resource list
     */
    public final List <LoadBalancedResource< RSC >> getResourceList() {
        return this.resources;
    }

    /**
     * Access totalCount
     *
     * @return total count
     */
    public final double getTotalCount() {
        return totalCount;
    }

    /**
     * Update total count
     *
     * @param totalCount new value for total count
     */
    public final void setTotalCount(final int totalCount) {
        this.totalCount = totalCount;
    }

    /**
     * Access load-balancing policy
     *
     * @return the mode
     */
    public final LoadBalancingPolicy getMode() {
        return loadBalancingPolicy;
    }

    /**
     * Update resources
     *
     * @param resourceList resource list
     */
    public final void setResources(final List <LoadBalancedResource< RSC >> resourceList) {
        this.resources = resourceList;
    }

    /**
     * Access unavailability period
     *
     * @return the unavailabilityPeriod
     */
    public final int getUnavailabilityPeriod() {
        return unavailabilityPeriod;
    }

    /**
     * Update unavailability period
     *
     * @param unavailabilityPeriod the unavailabilityPeriod to set
     */
    public final void setUnavailabilityPeriod(final int unavailabilityPeriod) {
        this.unavailabilityPeriod = unavailabilityPeriod;
    }

    /**
     * Access unavailableCount.
     *
     * @return unavailableCount value
     */
    public final int getUnavailableCount() {
        return unavailableCount;
    }

    /**
     * Update unavailableCount
     *
     * @param unavailableCount new unavailableCount value
     */
    public final void setUnavailableCount(final int unavailableCount) {
        this.unavailableCount = unavailableCount;
    }

    /**
     * Access global count.
     *
     * @return the globalCount
     */
    public final double getGlobalCount() {
        return globalCount;
    }

}
