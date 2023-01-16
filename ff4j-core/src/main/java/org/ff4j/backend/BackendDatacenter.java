package org.ff4j.backend;

import java.util.List;
import java.util.stream.Collectors;

import org.ff4j.loadbalancer.LoadBalancedResource;
import org.ff4j.loadbalancer.LoadBalancer;
import org.ff4j.loadbalancer.LoadBalancingPolicy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Implementing Load-balancing in between backends of the same DC.
 */
public class BackendDatacenter {

    /** Create a logger. */
    private static final Logger LOGGER = LoggerFactory.getLogger(BackendDatacenter.class);

    /** mark the dc as unavailable. */
    private boolean available = true;

    /** datacenter name. */
    private final String datacenterName;

    /** Inside a single datacenter I will have multiple Stargate Nodes. We will load-balance our queries among those instances. */
    private final LoadBalancer<Backend> backendLoadBalancer;

    /**
     * Gets available
     *
     * @return value of available
     */
    public boolean isAvailable() {
        return available;
    }

    /**
     * Gets available
     *
     * @return value of available
     */
    public boolean isNotAvailable() {
        return !available;
    }

    /**
     * Full constructor.
     *
     * @param datacenterName
     *      current dc name
     */
    public BackendDatacenter(String datacenterName, List<Backend> backendList) {
       this.datacenterName  = datacenterName;
        LOGGER.info(datacenterName + ":" + backendList);
       // Map each backend with
       this.backendLoadBalancer = new LoadBalancer<>(LoadBalancingPolicy.ROUND_ROBIN, backendList
               .stream()
               .map(backend -> {
                   LoadBalancedResource<Backend> lbRsc = new LoadBalancedResource<>(backend);
                   lbRsc.setId(backend.getId());
                   lbRsc.setDefaultWeight(100d / backendList.size());
                   lbRsc.setCurrentWeight(lbRsc.getDefaultWeight());
                   lbRsc.setAvailable(true);
                   lbRsc.setNbUse(0);
                   return lbRsc;
                })
               .collect(Collectors.toList()));
    }

    /**
     * Set value for available
     *
     * @param available new value for available
     */
    public void setAvailable(boolean available) {
        this.available = available;
    }

    /**
     * Getter accessor for attribute 'stargateNodesLB'.
     *
     * @return
     *       current value of 'stargateNodesLB'
     */
    public LoadBalancer<Backend> getBackendLoadBalancer() {
        return backendLoadBalancer;
    }

    /**
     * Getter accessor for attribute 'datacenterName'.
     *
     * @return
     *       current value of 'datacenterName'
     */
    public String getDatacenterName() {
        return datacenterName;
    }

    @Override
    public String toString() {
        return "ManagedServiceDatacenter{" +
                "available=" + available +
                ", datacenterName='" + datacenterName + '\'' +
                ", serviceLoadBalancer=" + backendLoadBalancer +
                '}';
    }
}
