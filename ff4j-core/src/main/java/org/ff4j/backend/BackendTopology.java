package org.ff4j.backend;

import org.ff4j.loadbalancer.LoadBalancedResource;
import org.ff4j.loadbalancer.exception.NoneResourceAvailableException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Implementing fail-over cross region.
 */
public class BackendTopology {

    /** Logger the client side LB and fail over. */
    private static final Logger LOGGER = LoggerFactory.getLogger(BackendTopology.class);

    /**
     * List of clients to interact with Backends
     *
     * If multiple datacenter provided the datacenter name will use as an EXECUTION PROFILE
     * to SWITCH contact points or cloud secure bundle.
     **/
    private final Map<String, BackendDatacenter> datacenters = new HashMap<>();

    /** Working with a currentDC. */
    private String currentDatacenter;

    /**
     * Initialize a managed topology from its definition.
     * @param sDeploy
     *      deployment
     */
    public BackendTopology(String localDc, Map<String, List<Backend>> sDeploy) {
        currentDatacenter = localDc;
        if (sDeploy !=null) {
            sDeploy.forEach((k, v) -> datacenters.put(k, new BackendDatacenter(k, v)));
        }
        if (currentDatacenter == null)
            currentDatacenter = datacenters.keySet().iterator().next();
    }

    // ------------------------------------------------
    // -- Load Balancing & fail-over    ---------------
    // ------------------------------------------------

    /**
     * Implementing fail-over cross DC .
     *
     * @param datacenter
     *      target datacenter
     */
    public void useDataCenter(String datacenter) {
        if (!getDatacenters().containsKey(datacenter)) {
            throw new IllegalArgumentException("'" + datacenter + "' is not a known datacenter please provides one "
                    + "in " + getDatacenters().keySet());
        }
        LOGGER.info("Using DataCenter [" + datacenter + "]");
        this.currentDatacenter = datacenter;
    }
    /**
     * Provide the current Datacenter client.
     *
     * @return
     *      the client for the current DC
     */
    public BackendDatacenter getLocalDatacenterClient() {
        if (!datacenters.containsKey(currentDatacenter)) {
            throw new IllegalStateException("Cannot retrieve datacenter [" + currentDatacenter + "] from definition, check cluster topology");
        }
        return datacenters.get(currentDatacenter);
    }

    /**
     * Retrieve an Api Rest URL still available in current DC or fail-over.
     *
     * @return
     *      an APi Rest URL available
     */
    public LoadBalancedResource<Backend> lookupBackend() {
        return getLocalDatacenterClient()   // Retrieve the current Dc based on localDc property
                .getBackendLoadBalancer()       // Retrieve the load-balancer for node
                .getLoadBalancedResource(); // Get a resource, idea is to invalidate resource if KO
    }

    /**
     * Failing over from one DC to another
     */
    public void failOverDatacenter() {
        getDatacenters().get(currentDatacenter).setAvailable(false);
        Set<String> availableDc = getAvailableDatacenters();
        if (availableDc.size() == 0) {
            throw new NoneResourceAvailableException("No Resource available anymore on ");
        }
        // Pick one and fail over
        String newDc = availableDc.iterator().next();
        LOGGER.info("Fail-over from {} to {}", currentDatacenter, newDc);
        useDataCenter(newDc);
    }

    /**
     * Failing over from one backend node to another.
     *
     * @param lb
     *      current resource to be disabled
     * @param t
     *      source error
     */
    public void failOverBackend(LoadBalancedResource<Backend> lb, Throwable t) {
        getLocalDatacenterClient().getBackendLoadBalancer().handleComponentError(lb, t);
    }

    /**
     * Gets datacenters
     *
     * @return value of datacenters
     */
    public Map<String, BackendDatacenter> getDatacenters() {
        return datacenters;
    }

    /**
     * Get available DC.
     *
     * @return
     *      available dc
     */
    public Set<String> getUnavailableDatacenters() {
        return datacenters.values()
                   .stream()
                   .filter(BackendDatacenter::isNotAvailable)
                   .map(BackendDatacenter::getDatacenterName)
                   .collect(Collectors.toSet());
    }

    /**
     * Get available DC.
     *
     * @return
     *      available dc
     */
    public Set<String> getAvailableDatacenters() {
        return datacenters.values()
                .stream()
                .filter(BackendDatacenter::isAvailable)
                .map(BackendDatacenter::getDatacenterName)
                .collect(Collectors.toSet());
    }

    @Override
    public String toString() {
        return "ManagedServiceDeployment{" +
                "datacenters=" + datacenters +
                ", currentDatacenter='" + currentDatacenter + '\'' +
                '}';
    }
}
