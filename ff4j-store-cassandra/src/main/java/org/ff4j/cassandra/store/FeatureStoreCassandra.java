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
import static org.ff4j.cassandra.CassandraConstants.COLUMN_FAMILY_FEATURES;
import static org.ff4j.cassandra.CassandraConstants.COL_FEAT_GROUPNAME;
import static org.ff4j.cassandra.CassandraConstants.COL_FEAT_UID;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.ff4j.cassandra.CassandraConnection;
import org.ff4j.cassandra.CassandraMapper;
import org.ff4j.cassandra.CassandraQueryBuilder;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.property.Property;
import org.ff4j.store.AbstractFeatureStore;
import org.ff4j.utils.JsonUtils;
import org.ff4j.utils.Util;

import com.datastax.driver.core.ResultSet;
import com.datastax.driver.core.Row;

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
    private CassandraQueryBuilder builder;
            
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
    public void createSchema() {
       // Roles & custom properties will be in the same column family  
       if (!conn.isColumnFamilyExist(COLUMN_FAMILY_FEATURES)) {
           
           // Create table
           conn.getSession().execute(getBuilder().cqlCreateColumnFamilyFeature());
           
           // Add a secondary index to query
           conn.getSession().execute(getBuilder().cqlCreateIndexGroupName());
       }
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean exist(String uid) {
        Util.assertHasLength(uid);
        return 1 == conn.getSession()
                .execute(getBuilder().cqlExistFeature(), uid)
                .iterator().next().getLong(0);
    }
    
    /** {@inheritDoc} */
    @Override
    public void enable(String uid) {
        assertFeatureExist(uid);
        conn.getSession().execute(getBuilder().cqlEnableFeature(), uid);
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String uid) {
        assertFeatureExist(uid);
        conn.getSession().execute(getBuilder().cqlDisableFeature(), uid);
    }    

    /** {@inheritDoc} */
    @Override
    public void create(Feature fp) {
        assertFeatureNotNull(fp);
        assertFeatureNotExist(fp.getUid());
        // Convert map<String, Property> to map<String, String>, structure in DB
        Map < String, String > mapOfProperties = new HashMap<String, String>();  
        if (fp.getCustomProperties() != null && !fp.getCustomProperties().isEmpty()) {
            for (Map.Entry<String, Property<?>> customP : fp.getCustomProperties().entrySet()) {
                if (customP.getValue() != null) {
                    mapOfProperties.put(customP.getKey(), customP.getValue().toJson());
                }
            }
        }
        conn.getSession().execute(getBuilder().cqlCreateFeature(), 
                fp.getUid(),
                fp.isEnable() ? 1 : 0, 
                fp.getDescription(), 
                JsonUtils.flippingStrategyAsJson(fp.getFlippingStrategy()),
                fp.getGroup(), fp.getPermissions(), mapOfProperties);
    }
    
    /** {@inheritDoc} */
    @Override
    public void delete(String uid) {
        assertFeatureExist(uid);
        conn.getSession().execute(getBuilder().cqlDeleteFeature(), uid);
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String uid) {
        assertFeatureExist(uid);
        ResultSet rs = conn.getSession().execute(getBuilder().cqlReadFeature(), uid);
        return CassandraMapper.mapFeature(rs.one());
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        Map < String, Feature> features = new HashMap<String, Feature>();
        ResultSet resultSet = conn.getSession().execute(getBuilder().selectAllFeatures());
        for (Row row : resultSet.all()) {
            Feature f = CassandraMapper.mapFeature(row);
            features.put(f.getUid(), f);
        }
        return features;
    }
    
    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        assertFeatureNotNull(fp);
        assertFeatureExist(fp.getUid());
        // easiest way to perform delta update (lot of attributes)
        delete(fp.getUid());
        create(fp);
    }

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String uid, String roleName) {
        assertFeatureExist(uid);
        Util.assertHasLength(roleName);
        conn.getSession().execute(getBuilder().cqlGrantRoleOnFeature(roleName), uid);
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String uid, String roleName) {
        assertFeatureExist(uid);
        Util.assertHasLength(roleName);
        // Read role from target feature
        ResultSet rs = conn.getSession().execute(getBuilder().cqlReadFeatureRoles(), uid);
        Set <String> permissions = CassandraMapper.mapFeaturePermissions(rs.one());
        // Remove expected
        permissions.remove(roleName);
        // Update new roleSet
        conn.getSession().execute(getBuilder().cqlUpdateFeatureRoles(), permissions, uid);
    }
    
    /** {@inheritDoc} */
    @Override
    public void enableGroup(String groupName) {
        assertGroupExist(groupName);
        /* Even with secondary index the 'update SET enable =1 WHERE GROUPNAME=?' does not work
         * We will update each feature one by one
         */
        ResultSet rs = conn.getSession().execute(
                getBuilder().cqlGetFeaturesNamesOfAGroup(), groupName);
        for (Row row : rs.all()) {
            enable(row.getString(COL_FEAT_UID));
        }
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
        assertGroupExist(groupName);
        ResultSet rs = conn.getSession().execute(
                getBuilder().cqlGetFeaturesNamesOfAGroup(), groupName);
        for (Row row : rs.all()) {
            disable(row.getString(COL_FEAT_UID));
        }
    }

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        Util.assertHasLength(groupName);
        return 0 != conn.getSession()
                .execute(getBuilder().cqlExistGroup(), groupName)
                .iterator().next().getLong(0);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        assertGroupExist(groupName);
        Map<String, Feature> result = new HashMap<String, Feature>();
        ResultSet rs = conn.getSession()
                .execute(getBuilder().cqlGetFeaturesOfAGroup(), groupName);
        for (Row row : rs.all()) {
            Feature f = CassandraMapper.mapFeature(row);
            result.put(f.getUid(), f);
        }
        return result;
    }

    /** {@inheritDoc} */
    @Override
    public void addToGroup(String uid, String groupName) {
        assertFeatureExist(uid);
        Util.assertHasLength(groupName);
        conn.getSession().execute(getBuilder().cqlAddFeatureToGroup(), groupName, uid);
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String uid, String groupName) {
        assertFeatureExist(uid);
        assertGroupExist(groupName);
        conn.getSession().execute(getBuilder().cqlRemoveFeatureFromGroup(), uid);
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> readAllGroups() {
        ResultSet rs = conn.getSession().execute(getBuilder().cqlGetGroups());
        Set< String > groups = new HashSet<String>();
        for (Row row : rs.all()) {
            groups.add(row.getString(COL_FEAT_GROUPNAME));
        }
        groups.remove(null);
        groups.remove("");
        return groups;
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        conn.getSession().execute(getBuilder().cqlTruncateFeatures());
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

    /**
     * Getter accessor for attribute 'builder'.
     *
     * @return
     *       current value of 'builder'
     */
    public CassandraQueryBuilder getBuilder() {
        if (builder == null) {
            builder = new CassandraQueryBuilder(conn);
        }
        return builder;
    }
}
