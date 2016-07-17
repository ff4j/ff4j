package org.ff4j.cassandra;

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
        this(Cluster.builder().addContactPoint(CassandraConstants.DEFAULT_HOST).build());
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
    
    /**
     * Read CQL from ddl-create file and initialize DB.
     */
    public void createSchema() {
        // TODO
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
