package org.ff4j.consul;

/*-
 * #%L
 * ff4j-store-consul
 * %%
 * Copyright (C) 2013 - 2024 FF4J
 * %%
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.ff4j.audit.Event;
import org.ff4j.consul.store.ConsulKeyBuilder;
import org.ff4j.store.kv.KeyValueDriver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.base.Optional;
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
public class ConsulConnection implements KeyValueDriver< String, String > {

    /** logger for this class. */
    private static final Logger LOGGER = LoggerFactory.getLogger(ConsulConnection.class);
    
    /** Repository. */
    private static final String DICTIONARY_SEPARATOR = ",";
    
    /** Default consult. */
    private Consul consul = null;
    
    /** Agent Client to register yourself in Consul. */
    private AgentClient agentClient;
    
    /** Access the health client. */
    private HealthClient healthClient;
    
    /** Access the key/value system. */
    private KeyValueClient keyValueClient;

    private final ConsulKeyBuilder keyBuilder;
    
    /** Default. */
    public ConsulConnection() {
        this(Consul.builder().build());
    }
    
    /**
     * Constructor with existing configuration.
     *
     * @param definedConsul
     *      target consul
     */
    public ConsulConnection(Consul definedConsul) {
        this(definedConsul, new ConsulKeyBuilder());
    }

    /**
     * Constructor with existing configuration and keyBuilder.
     *
     * @param definedConsul target consul
     * @param keyBuilder the key builder
     */
    public ConsulConnection(Consul definedConsul, ConsulKeyBuilder keyBuilder) {
        if (definedConsul == null) {
            throw new IllegalArgumentException("Consul settings cannot be null");
        }
        this.consul = definedConsul;
        this.keyBuilder = keyBuilder;
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
    @Override
    public void deleteKey(String key) {
        getKeyValueClient().deleteKey(key);
    }
    
    /** {@inheritDoc} */
    @Override
    public String getValue(String key) {
        var keyValue = getKeyValueClient().getValueAsString(key);
        if (keyValue.isEmpty()) {
            throw new IllegalArgumentException("Cannot read key '" + key + "' from consul. This error might error if you "
                    + "remove the KEY from consul without editing FF4J/FEATURES_DICTIONARY.");
        }
        return keyValue.get();
    } 
    
    /** {@inheritDoc} */
    @Override
    public void putValue(String key, String value) {
        getKeyValueClient().putValue(key, value);
    }
    
    // -- Features --
    
    /** {@inheritDoc} */
    public String getFeatureKey(String featureName) {
        return keyBuilder.getKeyName(featureName);
    }

    /** {@inheritDoc} */
    @Override
    public String getFeatureName(String key) {
        return keyBuilder.getFeatureName(key);
    }
    
    /** {@inheritDoc} */
    @Override
    public Set<String> getFeatureList() {
        var listFeatures = getKeyValueClient().getValueAsString(keyBuilder.getFeaturesDictionaryKey());
      return listFeatures.map(s -> new HashSet<>(Arrays.asList(s.split(DICTIONARY_SEPARATOR))))
          .orElseGet(HashSet::new);
    }
    
    /** {@inheritDoc} */
    @Override
    public void registerFeature(String featureName) {
        Set < String > featureList = getFeatureList();
        featureList.add(featureName);
        getKeyValueClient().putValue(keyBuilder.getFeaturesDictionaryKey(),
                String.join(DICTIONARY_SEPARATOR, featureList));
    }

    /** {@inheritDoc} */
    @Override
    public void unregisterFeature(String featureName) {
        Set < String > featureList = getFeatureList();
        featureList.remove(featureName);
        getKeyValueClient().putValue(keyBuilder.getFeaturesDictionaryKey(),
                String.join(DICTIONARY_SEPARATOR, featureList));
    }

    // -- Properties --
    
    /** {@inheritDoc} */
    public String getPropertyKey(String propertyName) {
        return keyBuilder.getPropertyKey(propertyName);
    }

    /** {@inheritDoc} */
    @Override
    public String getPropertyName(String key) {
        return keyBuilder.getPropertyName(key);
    }
    
    /** {@inheritDoc} */
    @Override
    public Set<String> getPropertyList() {
        var listProperties = getKeyValueClient().getValueAsString(keyBuilder.getPropertiesDictionaryKey());
      return listProperties.map(s -> new HashSet<>(Arrays.asList(
          s.split(DICTIONARY_SEPARATOR)))).orElseGet(HashSet::new);
    }
    
    /** {@inheritDoc} */
    @Override
    public void registerProperty(String propertyName) {
        Set < String > propertyList = getPropertyList();
        propertyList.add(propertyName);
        getKeyValueClient().putValue(keyBuilder.getPropertiesDictionaryKey(),
                String.join(DICTIONARY_SEPARATOR, propertyList));
    }
    
    /** {@inheritDoc} */
    @Override
    public void unregisterProperty(String propertyName) {
        Set < String > propertyList = getPropertyList();
        propertyList.remove(propertyName);
        getKeyValueClient().putValue(keyBuilder.getPropertiesDictionaryKey(),
                String.join(DICTIONARY_SEPARATOR, propertyList));
    }
    
    // Audit

    /** {@inheritDoc} */
    @Override
    public String getHitCountKey(Event e) {
        return keyBuilder.getHitCountKey(e);
    }

    /** {@inheritDoc} */
    @Override
    public String getMissKey(Event e) {
        return keyBuilder.getMissKey(e);
    }

    /** {@inheritDoc} */
    @Override
    public String getAuditTrailKey(Event e) {
        return keyBuilder.getAuditTrailKey(e);
    }
    
}
