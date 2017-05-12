package org.ff4j.consul;

/*
 * #%L
 * ff4j-store-consul
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.ff4j.audit.Event;
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
    private static final String KEY_DICTIONARY_FEATURE = "FF4J/FEATURES_DICTIONARY";
    
    /** Repository. */
    private static final String KEY_DICTIONARY_PROPERTY = "FF4J/PROPERTIES_DICTIONARY";
    
    /** Repository. */
    private static final String DICTIONARY_SEPARATOR = ",";
    
    /** audit key. */
    private static final SimpleDateFormat KDF = new SimpleDateFormat("yyyyMMdd");
    
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
    @Override
    public void deleteKey(String key) {
        getKeyValueClient().deleteKey(key);
    }
    
    /** {@inheritDoc} */
    @Override
    public String getValue(String key) {
        return getKeyValueClient().getValueAsString(key).get();
    } 
    
    /** {@inheritDoc} */
    @Override
    public void putValue(String key, String value) {
        getKeyValueClient().putValue(key, value);
    }
    
    // -- Features --
    
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
    @Override
    public Set<String> getFeatureList() {
        Optional < String> listFeatures = getKeyValueClient().getValueAsString(KEY_DICTIONARY_FEATURE);
        if (!listFeatures.isPresent()) return new HashSet<>();
        return new HashSet<>(Arrays.asList(listFeatures.get().split(DICTIONARY_SEPARATOR)));
    }
    
    /** {@inheritDoc} */
    @Override
    public void registerFeature(String featureName) {
        Set < String > featureList = getFeatureList();
        featureList.add(featureName);
        getKeyValueClient().putValue(KEY_DICTIONARY_FEATURE, 
                String.join(DICTIONARY_SEPARATOR, featureList));
    }

    /** {@inheritDoc} */
    @Override
    public void unregisterFeature(String featureName) {
        Set < String > featureList = getFeatureList();
        featureList.remove(featureName);
        getKeyValueClient().putValue(KEY_DICTIONARY_FEATURE, 
                String.join(DICTIONARY_SEPARATOR, featureList));
    }

    // -- Properties --
    
    /** {@inheritDoc} */
    public String getPropertyKey(String propertyName) {
        return ConsulConstants.FF4J_PREFIXKEY_PROPERTIES + propertyName;
    }

    /** {@inheritDoc} */
    @Override
    public String getPropertyName(String key) {
        return key.replaceAll(ConsulConstants.FF4J_PREFIXKEY_PROPERTIES, "");
    }
    
    /** {@inheritDoc} */
    @Override
    public Set<String> getPropertyList() {
        Optional < String> listProperties = getKeyValueClient().getValueAsString(KEY_DICTIONARY_PROPERTY);
        if (!listProperties.isPresent())  return new HashSet<>();
        return new HashSet<>(Arrays.asList(
                listProperties.get().split(DICTIONARY_SEPARATOR)));
    }
    
    /** {@inheritDoc} */
    @Override
    public void registerProperty(String propertyName) {
        Set < String > propertyList = getPropertyList();
        propertyList.add(propertyName);
        getKeyValueClient().putValue(KEY_DICTIONARY_PROPERTY, 
                String.join(DICTIONARY_SEPARATOR, propertyList));
    }
    
    /** {@inheritDoc} */
    @Override
    public void unregisterProperty(String propertyName) {
        Set < String > propertyList = getPropertyList();
        propertyList.remove(propertyName);
        getKeyValueClient().putValue(KEY_DICTIONARY_PROPERTY, 
                String.join(DICTIONARY_SEPARATOR, propertyList));
    }
    
    // Audit

    /** {@inheritDoc} */
    @Override
    public String getHitCountKey(Event e) {
        return ConsulConstants.FF4J_PREFIXKEY_HITS + "/" + 
                    KDF.format(e.getTimestamp()) + "/"   +
                    e.getName() + "/" + e.getUuid();
    }

    /** {@inheritDoc} */
    @Override
    public String getMissKey(Event e) {
        return ConsulConstants.FF4J_PREFIXKEY_MISS + "/" + 
                KDF.format(e.getTimestamp()) + "/"   +
                e.getName() + "/" + e.getUuid();
    }

    /** {@inheritDoc} */
    @Override
    public String getAuditTrailKey(Event e) {
        return ConsulConstants.FF4J_PREFIXKEY_AUDIT + "/" + 
                KDF.format(e.getTimestamp()) + "/"   +
                e.getName() + "/" + e.getUuid();
    }
    
}
