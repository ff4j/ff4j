package org.ff4j.cassandra.store;

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


import static org.ff4j.cassandra.CassandraConstants.CQL_EXIST_FEATURE;

import java.util.Map;
import java.util.Set;

import org.ff4j.cassandra.CassandraConnection;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.store.AbstractFeatureStore;
import org.ff4j.utils.Util;
/**
 * Implementation of {@link FeatureStore} to work with Cassandra Storage.
 * 
 * Minimize the Number of Writes : 
 * Writes in Cassandra aren’t free, but they’re awfully cheap. Cassandra is optimized for high write throughput, 
 * and almost all writes are equally efficient [1]. If you can perform extra writes to improve the efficiency of
 * your read queries, it’s almost always a good tradeoff. Reads tend to be more expensive and are much more 
 * difficult to tune.
 * 
 * Minimize Data Duplication
 * Denormalization and duplication of data is a fact of life with Cassandra. Don’t be afraid of it. Disk space 
 * is generally the cheapest resource (compared to CPU, memory, disk IOPs, or network), and Cassandra is 
 * architected around that fact. In order to get the most efficient reads, you often need to duplicate data.
 * 
 * Rule 1: Spread Data Evenly Around the Cluster
 * Rule 2: Minimize the Number of Partitions Read
 * 
 * Ces familles de colonnes contiennent des colonnes ainsi qu'un ensemble de colonnes connexes qui sont identifiées par une clé de ligne
 * 
 *
 * @author Cedrick Lunven (@clunven)
 */
public class FeatureStoreCassandra extends AbstractFeatureStore {
    
    /** Connection to store Cassandra. */
    private CassandraConnection conn;
    
    /**
     * Default constructor.
     */
    public FeatureStoreCassandra() {
    }
    
    /**
     * Initialization through {@link CassandraConnection}.
     *
     * @param conn
     *      current client to cassandra db
     */
    public FeatureStoreCassandra(CassandraConnection conn) {
        this.conn = conn;
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String uid) {
        Util.assertHasLength(uid);
        return 1 == conn.getSession().execute(CQL_EXIST_FEATURE, uid) //
                        .iterator().next() //
                        .getInt("NB");
    }
    
    /** {@inheritDoc} */
    @Override
    public void enable(String uid) {
        Util.assertHasLength(uid);
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        
        // update
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String fId) {
        throw new UnsupportedOperationException("Not yet ready");
    }

    

    /** {@inheritDoc} */
    @Override
    public void create(Feature fp) {
        throw new UnsupportedOperationException("Not yet ready");
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String featureUid) {
        throw new UnsupportedOperationException("Not yet ready");
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String fpId) {
    }

    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
    }

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String flipId, String roleName) {
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String flipId, String roleName) {
    }

    /** {@inheritDoc} */
    @Override
    public void enableGroup(String groupName) {
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
    }

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        return false;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void addToGroup(String featureId, String groupName) {
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String featureId, String groupName) {
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> readAllGroups() {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
    }

    /**
     * Getter accessor for attribute 'conn'.
     *
     * @return
     *       current value of 'conn'
     */
    public CassandraConnection getConn() {
        return conn;
    }

    /**
     * Setter accessor for attribute 'conn'.
     * @param conn
     * 		new value for 'conn '
     */
    public void setConn(CassandraConnection conn) {
        this.conn = conn;
    }

    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        // TODO Auto-generated method stub
        
    }
}
