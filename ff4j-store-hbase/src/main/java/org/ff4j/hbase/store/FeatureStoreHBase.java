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
import org.ff4j.hbase.HBaseQueryBuilder;
import org.ff4j.hbase.mapper.HBaseFeatureMapper;
import org.ff4j.store.AbstractFeatureStore;
import org.ff4j.utils.Util;

/**
 * Implementation of {@link FeatureStore} to work with HBASE.
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
