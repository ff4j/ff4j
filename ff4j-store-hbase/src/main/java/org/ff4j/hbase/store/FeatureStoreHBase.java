package org.ff4j.hbase.store;

import static org.ff4j.hbase.HBaseConstants.B_FEATURES_CF_CORE;
import static org.ff4j.hbase.HBaseConstants.B_FEATURES_CF_PROPERTIES;
import static org.ff4j.hbase.HBaseConstants.FEATURES_CF_CORE;
import static org.ff4j.hbase.HBaseConstants.FEATURES_CF_PROPERTIES;
import static org.ff4j.hbase.HBaseConstants.FEATURES_TABLENAME;
import static org.ff4j.hbase.HBaseConstants.FEATURES_TABLENAME_ID;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.hadoop.hbase.client.Connection;
import org.apache.hadoop.hbase.client.ConnectionFactory;
import org.apache.hadoop.hbase.client.Delete;
import org.apache.hadoop.hbase.client.Put;
import org.apache.hadoop.hbase.client.Result;
import org.apache.hadoop.hbase.client.ResultScanner;
import org.apache.hadoop.hbase.client.Scan;
import org.apache.hadoop.hbase.client.Table;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.hbase.HBaseConnection;
import org.ff4j.hbase.mapper.HBaseFeatureMapper;
import org.ff4j.hbase.mapper.HBaseQueryBuilder;
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
public class FeatureStoreHBase extends AbstractFeatureStore {
    
    /** Mapper. */
    private static final HBaseFeatureMapper MAPPER = new HBaseFeatureMapper();
    
    /** Connection to store Cassandra. */
    private HBaseConnection conn;
    
    /**
     * Default constructor.
     */
    public FeatureStoreHBase() {
    }
    
    /**
     * Initialization through {@link HBaseConnection}.
     *
     * @param conn
     *      current client to cassandra db
     */
    public FeatureStoreHBase(HBaseConnection conn) {
        this.conn = conn;
    }

     
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        conn.createTable(FEATURES_TABLENAME_ID, Util.set(FEATURES_CF_CORE, FEATURES_CF_PROPERTIES));
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean exist(String featId) {
        Util.assertHasLength(featId);
        try (Connection hbConn = ConnectionFactory.createConnection(conn.getConfig())) {
            try(Table table = hbConn.getTable(FEATURES_TABLENAME)) {
                return !table.get(HBaseQueryBuilder.getFeatureById(featId)).isEmpty();
            }
        } catch (IOException e) {
            throw new FeatureAccessException("Cannot check feature existence", e);
        }
    }
    
    private void executePutCommand(Put putQuery) {
        try (Connection hbConn = ConnectionFactory.createConnection(conn.getConfig())) {
            try(Table table = hbConn.getTable(FEATURES_TABLENAME)) {
                table.put(putQuery);
            }
        } catch (IOException e) {
            throw new FeatureAccessException("Cannot execute command", e);
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public void create(Feature fp) {
        assertFeatureNotNull(fp);
        assertFeatureNotExist(fp.getUid());
        executePutCommand(MAPPER.toStore(fp));
    }

    /** {@inheritDoc} */
    @Override
    public void enable(String uid) {
        assertFeatureExist(uid);
        executePutCommand(HBaseQueryBuilder.queryEnableFeature(uid));
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String uid) {
        assertFeatureExist(uid);
        executePutCommand(HBaseQueryBuilder.queryDisableFeature(uid));
    }    
    
    /** {@inheritDoc} */
    @Override
    public Feature read(String uid) {
        assertFeatureExist(uid);
        try (Connection hbConn = ConnectionFactory.createConnection(conn.getConfig())) {
            try(Table table = hbConn.getTable(FEATURES_TABLENAME)) {
                Result result = table.get(HBaseQueryBuilder.getFeatureById(uid));
                return MAPPER.fromStore(result);
            }
        } catch (IOException e) {
            throw new FeatureAccessException("Cannot check feature existence", e);
        }
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        Map<String, Feature> mapOfFeature = new HashMap<>();
        try (Connection hbConn = ConnectionFactory.createConnection(conn.getConfig())) {
            try(Table table = hbConn.getTable(FEATURES_TABLENAME)) {
                
                Scan scan = new Scan();
                scan.setCaching(100);
                scan.setBatch(100);
                scan.addFamily(B_FEATURES_CF_CORE);
                scan.addFamily(B_FEATURES_CF_PROPERTIES);
                
                try(ResultScanner resultScanner = table.getScanner(scan)) {
                    Iterator<Result> iterator = resultScanner.iterator();
                    while (iterator.hasNext()) {
                        Feature f = MAPPER.fromStore(iterator.next());
                        mapOfFeature.put(f.getUid(), f);
                    }
                }
            }
        } catch (IOException e) {
            throw new FeatureAccessException("Cannot read all features", e);
        }
        return mapOfFeature;
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String uid) {
        assertFeatureExist(uid);
        try (Connection hbConn = ConnectionFactory.createConnection(conn.getConfig())) {
            try(Table table = hbConn.getTable(FEATURES_TABLENAME)) {
                List<Delete> list = new ArrayList<Delete>();
                Delete del = new Delete(uid.getBytes());
                list.add(del);
                table.delete(list);
            }
        } catch (IOException e) {
            throw new FeatureAccessException("Cannot delete feature ", e);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        assertFeatureNotNull(fp);
        assertFeatureExist(fp.getUid());
        delete(fp.getUid());
        create(fp);
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
        conn.truncateTable(FEATURES_TABLENAME_ID);
    }
    
    /**
     * Getter accessor for attribute 'conn'.
     *
     * @return
     *       current value of 'conn'
     */
    public HBaseConnection getConn() {
        return conn;
    }

    /**
     * Setter accessor for attribute 'conn'.
     *
     * @param conn
     *      new value for 'conn '
     */
    public void setConn(HBaseConnection conn) {
        this.conn = conn;
    }
}
