package org.ff4j.feature.togglestrategy;

import org.ff4j.backend.BackendSupport;
import org.ff4j.feature.Feature;
import org.ff4j.property.evaluate.FF4jEvaluationContext;
import org.ff4j.property.list.PropertyListString;
import org.ff4j.utils.Assert;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.List;

/**
 * Check hostName and return true if part of the expected server list 'grantedClients'
 */
public class HostNameWhiteListToggleStrategy extends AbstractToggleStrategy {

	/** Params. */
    public static final String WHITE_LIST = "whitelist";

    /**
     * Constructor with everything needed
     *
     * @param backend
     *      current backend
     * @param relatedFeature
     *      current feature
     * @param config
     *      configuration
     */
    public HostNameWhiteListToggleStrategy(BackendSupport backend, Feature relatedFeature, FF4jEvaluationContext config) {
        this(backend, relatedFeature, (List<String>) config.getProperty(WHITE_LIST).getValue());
    }

    /**
     * Constructor with everything needed
     *
     * @param backend
     *      current backend
     * @param relatedFeature
     *      current feature
     * @param hosts
     *      list of hosts
     */
    public HostNameWhiteListToggleStrategy(BackendSupport backend, Feature relatedFeature, List<String> hosts) {
        super(backend, relatedFeature, new FF4jEvaluationContext());
        Assert.assertNotNull(hosts);
        Assert.assertTrue(hosts.size() > 0);
        getConfig().putProperty(new PropertyListString(WHITE_LIST, hosts.toArray(new String[0])));
    }

    /**
     * Constructor with everything needed
     *
     * @param backend
     *      current backend
     * @param relatedFeature
     *      current feature
     * @param host
     *      list of hosts
     */
    public HostNameWhiteListToggleStrategy(BackendSupport backend, Feature relatedFeature, String... host) {
        this(backend, relatedFeature, Arrays.asList(host));
    }

    /** {@inheritDoc} */
    @Override
    public boolean test(FF4jEvaluationContext ff4jEvaluationContext) {
        PropertyListString pls = (PropertyListString) getConfig().getProperty(WHITE_LIST);
        try {
            return pls.getValue().contains(InetAddress.getLocalHost().getHostName());
        } catch (UnknownHostException e) {
            throw new IllegalArgumentException("Cannot find the target host by itself", e);
        }
    }

}
