package org.ff4j.cassandra;

import com.datastax.driver.core.Cluster;
import com.datastax.driver.core.Session;

/**
 * Connection to Cassandra.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class CassandraConnection {
    
    private Cluster cluster;
    
    private Session session;

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
     * Setter accessor for attribute 'cluster'.
     * @param cluster
     * 		new value for 'cluster '
     */
    public void setCluster(Cluster cluster) {
        this.cluster = cluster;
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
     * Setter accessor for attribute 'session'.
     * @param session
     * 		new value for 'session '
     */
    public void setSession(Session session) {
        this.session = session;
    }
    
    

}
