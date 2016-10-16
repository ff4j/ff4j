package org.ff4j.cassandra;

import static org.ff4j.cassandra.CassandraConstants.CQL_CREATEKEYSPACE;
import static org.ff4j.cassandra.CassandraConstants.DEFAULT_KEYSPACE;
import static org.ff4j.cassandra.CassandraConstants.DEFAULT_REPLICATION_FACTOR;

import java.text.MessageFormat;

import org.ff4j.utils.Util;

/*
 * #%L
 * ff4j-store-cassandra
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.datastax.driver.core.Cluster;
import com.datastax.driver.core.Cluster.Builder;
import com.datastax.driver.core.Host;
import com.datastax.driver.core.KeyspaceMetadata;
import com.datastax.driver.core.Metadata;
import com.datastax.driver.core.Session;
import com.datastax.driver.core.TableMetadata;

/**
 * Connection to Cassandra.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class CassandraConnection {
    
    /** logger for this class. */
    private static final Logger LOGGER = LoggerFactory.getLogger(CassandraConnection.class);
    
    /** Cassandra cluster. */
    private final Cluster cluster;
    
    /** Cassandra Session. */
    private Session session;
    
    /** Cassandra usersanem. */
    private String userName;
    
    /** Target keySpace. */
    private String keySpace = DEFAULT_KEYSPACE;
    
    /** current replication factor. */
    private int replicationFactor = DEFAULT_REPLICATION_FACTOR;
    
    /** Default. */
    public CassandraConnection() {
        this(CassandraConstants.DEFAULT_HOST, CassandraConstants.PORT_CQL_NATIVE, null, null);
    }
    
    /**
     * Init with login and password.
     * 
     * @param userName
     *      user name to connect to DB
     * @param password
     *      password to connect to DB
     */
    public CassandraConnection(String userName, String password) {
        this(CassandraConstants.DEFAULT_HOST, CassandraConstants.PORT_CQL_NATIVE, userName, password);
    }
    
    /**
     * Initialization of host and port.
     * 
     * @param host
     *      current host
     * @param port
     *      current port
     */
    public CassandraConnection(String host, int port) {
        this(host, port, null, null);
    }
    
    /**
     * Initialization of host and port.
     * 
     * @param host
     *      current host
     * @param port
     *      current port
     */
    public CassandraConnection(String host, int port, String userName, String password) {
        Builder builder = Cluster.builder().addContactPoint(host).withPort(port);
        if (Util.hasLength(userName)) {
            this.userName = userName;
            builder.withCredentials(userName, password);
        }
        this.cluster = builder.build();
        initCluster();
    }
    
    /**
     * Initialization with cluster.
     *
     * @param definedCluster
     */
    public CassandraConnection(Cluster definedCluster) {
        this.cluster = definedCluster;
        initCluster();
    }
   
    /**
     * Init Cassandra session from Cluster.s
     */
    private void initCluster() {
        if (null == cluster) {
            throw new IllegalStateException("Cluster Cassandra has not be well defined");
        }
        Metadata metadata = cluster.getMetadata();
        LOGGER.info("Connecting to cluster... '{}'",  metadata.getClusterName());
        for ( Host host : metadata.getAllHosts() ) {
            LOGGER.info("Datatacenter: '{}' Host: '{}' Rack '{}'", host.getDatacenter(), host.getAddress(), host.getRack());
        }
        this.session = cluster.connect();  
        LOGGER.info("Connection Successful.");
    }
    
    /**
     * Create keySpace with default value.
     */
    public void createKeySpace() {
        session.execute(MessageFormat.format(CQL_CREATEKEYSPACE, keySpace, replicationFactor));
    }
    
    /**
     * Drop element relative to FF4J.
     */
    public void dropSchema() {
        CassandraQueryBuilder cqb = new CassandraQueryBuilder(this);
        session.execute(cqb.cqlDropAudit());
        session.execute(cqb.cqlDropFeatures());
        session.execute(cqb.cqlDropProperties());
    }
    
    /**
     * Create keyspace.
     *
     * @param name
     *      id of the keyspace
     * @param replicationFactor
     *      replicator of the keyspace
     */
    public void createKeySpace(String name, int replicationFactor) {
        this.keySpace = name;
        this.replicationFactor = replicationFactor;
        this.createKeySpace();
    }
    
    /**
     * Check existence of a table in Cassandra.
     *
     * @param columnName
     *      current column family name
     * @return
     *      if the column exist
     */
    public boolean isColumnFamilyExist(String columnName) {
        KeyspaceMetadata ks = cluster.getMetadata().getKeyspace(getKeySpace());
        TableMetadata table = ks.getTable(columnName);
        return table != null;
    }
    
    /** Close cluster. */  
    public void close() {
       session.close();
       cluster.close();  
    }

    /**
     * Getter accessor for attribute 'cluster'.
     *
     * @return
     *       current value of 'cluster'
     */
    public Cluster getCluster() {
        return cluster;
    }

    /**
     * Getter accessor for attribute 'session'.
     *
     * @return
     *       current value of 'session'
     */
    public Session getSession() {
        return session;
    }

    /**
     * Getter accessor for attribute 'userName'.
     *
     * @return
     *       current value of 'userName'
     */
    public String getUserName() {
        return userName;
    }

    /**
     * Getter accessor for attribute 'keySpace'.
     *
     * @return
     *       current value of 'keySpace'
     */
    public String getKeySpace() {
        return keySpace;
    }

    /**
     * Setter accessor for attribute 'keySpace'.
     * @param keySpace
     * 		new value for 'keySpace '
     */
    public void setKeySpace(String keySpace) {
        this.keySpace = keySpace;
    }

    /**
     * Getter accessor for attribute 'replicationFactor'.
     *
     * @return
     *       current value of 'replicationFactor'
     */
    public int getReplicationFactor() {
        return replicationFactor;
    }

    /**
     * Setter accessor for attribute 'replicationFactor'.
     * @param replicationFactor
     * 		new value for 'replicationFactor '
     */
    public void setReplicationFactor(int replicationFactor) {
        this.replicationFactor = replicationFactor;
    }

}
