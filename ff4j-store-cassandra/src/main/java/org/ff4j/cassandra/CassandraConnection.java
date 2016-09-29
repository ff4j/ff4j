package org.ff4j.cassandra;

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
import com.datastax.driver.core.Host;
import com.datastax.driver.core.Metadata;
import com.datastax.driver.core.Session;

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
    
    /** Default. */
    public CassandraConnection() {
        this(CassandraConstants.DEFAULT_HOST));
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
        this(Cluster.builder().addContactPoint(host).withPort(port).build());
    }
    
    /**
     * Initialization of host.
     * 
     * @param host
     *      current host
     * @param port
     *      current port
     */
    public CassandraConnection(String host) {
        this(Cluster.builder().addContactPoint(host).build());
    }
    
    /**
     * Initialization with cluster.
     *
     * @param definedCluster
     */
    public CassandraConnection(Cluster definedCluster) {
        this.cluster = definedCluster;
        Metadata metadata = cluster.getMetadata();
        LOGGER.info("Connected to cluster: %s\n",  metadata.getClusterName());
        for ( Host host : metadata.getAllHosts() ) {
            LOGGER.info("Datatacenter: %s; Host: %s; Rack: %s\n", host.getDatacenter(), host.getAddress(), host.getRack());
        }
        this.session = cluster.connect();  
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

}
