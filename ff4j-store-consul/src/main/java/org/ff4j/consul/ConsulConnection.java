package org.ff4j.consul;

import org.ff4j.store.kv.KeyValueDriver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.orbitz.consul.AgentClient;
import com.orbitz.consul.Consul;
import com.orbitz.consul.HealthClient;
import com.orbitz.consul.KeyValueClient;

/**
 * Wrapping connection to consul to use it in different classes.
 * 
 * Server RPC (Default 8300). 
 *      This is used by servers to handle incoming requests from other agents. TCP only.
 * Serf LAN (Default 8301).
 *      This is used to handle gossip in the LAN. Required by all agents. TCP and UDP.
 * Serf WAN (Default 8302).
 *      This is used by servers to gossip over the WAN to other servers. TCP and UDP.
 * CLI RPC (Default 8400).
 *      This is used by all agents to handle RPC from the CLI, but is deprecated in Consul 0.8 and later. TCP only. In Consul 0.8 all CLI commands were changed to use the HTTP API and the RPC interface was completely removed.
 * HTTP API (Default 8500).
 *      This is used by clients to talk to the HTTP API. TCP only.
 * DNS Interface (Default 8600).
 *      Used to resolve DNS queries. TCP and UDP.
 * 
 * @author Cedrick LUNVEN (@clunven)
 */
public class ConsulConnection implements KeyValueDriver {

    /** logger for this class. */
    private static final Logger LOGGER = LoggerFactory.getLogger(ConsulConnection.class);
    
    /** Default consult. */
    private Consul consul = null;
    
    /** Agent Client to register yourself in Consul. */
    private AgentClient agentClient;
    
    /** Access the health client. */
    private HealthClient healthClient;
    
    /** Access the key/value system. */
    private KeyValueClient keyValueClient;
    
    /** Default. */
    public ConsulConnection() {
        this(Consul.builder().build());
    }
    
    /**
     * Constructor with existing configuration.
     *
     * @param definedConsule
     *      target consul
     */
    public ConsulConnection(Consul definedConsul) {
        if (definedConsul == null) {
            throw new IllegalArgumentException("Consul settings cannot be null");
        }
        this.consul = definedConsul;
        LOGGER.info("Consul clustered has been initialized " + consul.statusClient().getPeers());
    }

    /**
     * Getter accessor for attribute 'consul'.
     *
     * @return
     *       current value of 'consul'
     */
    public Consul getConsul() {
        return consul;
    }

    /**
     * Getter accessor for attribute 'agentClient'.
     *
     * @return
     *       current value of 'agentClient'
     */
    public AgentClient getAgentClient() {
        if (agentClient == null) {
            this.agentClient = consul.agentClient();
        }
        return agentClient;
    }

    /**
     * Getter accessor for attribute 'healthClient'.
     *
     * @return
     *       current value of 'healthClient'
     */
    public HealthClient getHealthClient() {
        if (healthClient == null) {
            this.healthClient   = consul.healthClient();
        }
        return healthClient;
    }

    /**
     * Getter accessor for attribute 'keyValueClient'.
     *
     * @return
     *       current value of 'keyValueClient'
     */
    public KeyValueClient getKeyValueClient() {
        if (keyValueClient == null) {
            this.keyValueClient = consul.keyValueClient();
        }
        return keyValueClient;
    }

    /** {@inheritDoc} */
    @Override
    public boolean existKey(String key) {
        return getKeyValueClient().getValue(key).isPresent();
    }

    /** {@inheritDoc} */
    public void deleteKey(String key) {
        getKeyValueClient().deleteKey(key);
    }
    
    /** {@inheritDoc} */
    public String getValue(String key) {
        return getKeyValueClient().getValueAsString(key).get();
    } 
    
    /** {@inheritDoc} */
    public void putValue(String key, String value) {
        getKeyValueClient().putValue(key, value);
    }
    
    /** {@inheritDoc} */
    public String getFeatureKey(String featureName) {
        return ConsulConstants.FF4J_PREFIXKEY_FEATURES + featureName;
    }

    /** {@inheritDoc} */
    @Override
    public String getFeatureName(String key) {
        return key.replaceAll(ConsulConstants.FF4J_PREFIXKEY_FEATURES, "");
    }
    
    /** {@inheritDoc} */
    public String getPropertyKey(String propertyName) {
        return ConsulConstants.FF4J_PREFIXKEY_PROPERTIES + propertyName;
    }

    /** {@inheritDoc} */
    @Override
    public String getPropertyName(String key) {
        return key.replaceAll(ConsulConstants.FF4J_PREFIXKEY_PROPERTIES, "");
    }
        
}
