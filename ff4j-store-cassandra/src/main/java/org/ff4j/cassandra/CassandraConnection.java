package org.ff4j.cassandra;

import static org.ff4j.cassandra.CassandraConstants.CQL_CREATEKEYSPACE;
import static org.ff4j.cassandra.CassandraConstants.DEFAULT_HOST;
import static org.ff4j.cassandra.CassandraConstants.DEFAULT_KEYSPACE;
import static org.ff4j.cassandra.CassandraConstants.DEFAULT_REPLICATION_FACTOR;
import static org.ff4j.cassandra.CassandraConstants.PORT_CQL_NATIVE;

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
    private Cluster cluster = null;
    
    /** Cassandra Session. */
    private Session session = null;
    
    /** Cassandra userusername. */
    private String userName = null;
    
    /** Cassandra password if authentication enabled. */
    private String userPassword = null;
    
    /** Cassandra server hostname. */
    private String hostName = DEFAULT_HOST;
    
    /** Cassandra server listening port. */
    private int port = PORT_CQL_NATIVE;
    
    /** Target keySpace. */
    private String keySpace = DEFAULT_KEYSPACE;
    
    /** current replication factor. */
    private int replicationFactor = DEFAULT_REPLICATION_FACTOR;
    
    /** Default. */
    public CassandraConnection() {
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
        this.userName     = userName;
        this.userPassword = password;
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
        this.hostName     = host;
        this.port         = port;
        this.userName     = userName;
        this.userPassword = password;
    }
    
    /**
     * Initialization with cluster.
     *
     * @param definedCluster
     */
    public CassandraConnection(Cluster definedCluster) {
        if (definedCluster == null) {
            throw new IllegalArgumentException("Cluster cannot be null");
        }
        this.cluster = definedCluster;
    }
   
    /**
     * Init Cassandra session from Cluster.s
     */
    public void initSession() {
        if (null == cluster) {
            Builder builder = Cluster.builder().addContactPoint(hostName).withPort(port);
            if (Util.hasLength(userName)) {
                builder.withCredentials(userName, userPassword);
            }
            this.cluster = builder.build();
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
        getSession().execute(MessageFormat.format(CQL_CREATEKEYSPACE, keySpace, replicationFactor));
    }
    
    /**
     * Drop element relative to FF4J.
     */
    public void dropSchema() {
        CassandraQueryBuilder cqb = new CassandraQueryBuilder(this);
        getSession().execute(cqb.cqlDropAudit());
        getSession().execute(cqb.cqlDropFeatures());
        getSession().execute(cqb.cqlDropProperties());
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
        KeyspaceMetadata ks = getCluster().getMetadata().getKeyspace(getKeySpace());
        TableMetadata table = ks.getTable(columnName);
        return table != null;
    }
    
    /** Close cluster. */  
    public void close() {
       getSession().close();
       getCluster().close();  
    }

    /**
     * Getter accessor for attribute 'cluster'.
     *
     * @return
     *       current value of 'cluster'
     */
    public Cluster getCluster() {
        if (cluster == null) {
            initSession();
        }
        return cluster;
    }

    /**
     * Getter accessor for attribute 'session'.
     *
     * @return
     *       current value of 'session'
     */
    public Session getSession() {
        if (session == null) {
            initSession();
        }
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

    /**
     * Getter accessor for attribute 'userPassword'.
     *
     * @return
     *       current value of 'userPassword'
     */
    public String getUserPassword() {
        return userPassword;
    }

    /**
     * Setter accessor for attribute 'userPassword'.
     * @param userPassword
     * 		new value for 'userPassword '
     */
    public void setUserPassword(String userPassword) {
        this.userPassword = userPassword;
    }

    /**
     * Getter accessor for attribute 'hostName'.
     *
     * @return
     *       current value of 'hostName'
     */
    public String getHostName() {
        return hostName;
    }

    /**
     * Setter accessor for attribute 'hostName'.
     * @param hostName
     * 		new value for 'hostName '
     */
    public void setHostName(String hostName) {
        this.hostName = hostName;
    }

    /**
     * Getter accessor for attribute 'port'.
     *
     * @return
     *       current value of 'port'
     */
    public int getPort() {
        return port;
    }

    /**
     * Setter accessor for attribute 'port'.
     * @param port
     * 		new value for 'port '
     */
    public void setPort(int port) {
        this.port = port;
    }

    /**
     * Setter accessor for attribute 'cluster'.
     * @param cluster
     * 		new value for 'cluster '
     */
    public void setCluster(Cluster cluster) {
        this.cluster = cluster;
    }

    /**
     * Setter accessor for attribute 'userName'.
     * @param userName
     * 		new value for 'userName '
     */
    public void setUserName(String userName) {
        this.userName = userName;
    }

}
