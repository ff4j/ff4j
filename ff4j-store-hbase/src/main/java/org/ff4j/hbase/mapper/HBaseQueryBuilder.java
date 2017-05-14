package org.ff4j.hbase.mapper;

import static org.ff4j.hbase.HBaseConstants.B_FEATURES_CF_CORE;
import static org.ff4j.hbase.HBaseConstants.B_FEAT_ENABLE;
import static org.ff4j.hbase.HBaseConstants.B_FEAT_UID;

import org.apache.hadoop.hbase.client.Get;
import org.apache.hadoop.hbase.client.Put;
import org.apache.hadoop.hbase.util.Bytes;

/**
 * Helper to create queries in HBASE.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class HBaseQueryBuilder {
    
    /**
     * Hide default constructor.
     */
    private HBaseQueryBuilder() {
    }
    
    /**
     * Search by rowKey, get only rowKey (exist).
     *
     * @param uid
     *      target uid
     * @return
     *      target query
     */
    public static Get getFeatureById(String uid) {
        return new Get(Bytes.toBytes(uid));
    }
    
    /**
     * Create query.
     *
     * @param uid
     *      current feature id
     * @return
     *      target query
     */
    public static Put queryEnableFeature(String uid) {
        Put put = new Put(Bytes.toBytes(uid));
        put.addColumn(B_FEATURES_CF_CORE, B_FEAT_UID, Bytes.toBytes(uid));
        put.addColumn(B_FEATURES_CF_CORE, B_FEAT_ENABLE, Bytes.toBytes(true));
        return put;
    }
    
    /**
     * Create query.
     *
     * @param uid
     *      current feature id
     * @return
     *      target query
     */
    public static Put queryDisableFeature(String uid) {
        Put put = new Put(Bytes.toBytes(uid));
        put.addColumn(B_FEATURES_CF_CORE, B_FEAT_UID, Bytes.toBytes(uid));
        put.addColumn(B_FEATURES_CF_CORE, B_FEAT_ENABLE, Bytes.toBytes(false));
        return put;
    }

}
