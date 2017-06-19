package org.ff4j.hbase;

import java.util.Set;

import org.apache.hadoop.hbase.TableName;
import org.apache.hadoop.hbase.util.Bytes;
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

/**
 * Constants to work with HBASE DB.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class HBaseConstants {
    
    // --- HBASE Settings -----
    
    /** configuration key. */
    public static final String ZOOKEEPER_QUORUM_PARAM  = "hbase.zookeeper.quorum";
    
    /** configuration value. */
    public static final String ZOOKEEPER_QUORUM_DEFAULT = "localhost";    
    
    /** configuration key. */
    public static final String ZOOKEEPER_CLIENTPORT_PARAM = "hbase.zookeeper.property.clientPort";
    
    /** configuration value. */
    public static final int ZOOKEEPER_CLIENTPORT_DEFAULT = 2181;
    
    
    public static final String MASTER_PORT_PARAM        = "hbase.master.port";
    
    public static final int    MASTER_PORT_DEFAULT      = 60000;    
    
    public static final String MASTER_PORTINFO_PARAM    = "hbase.master.info.port";
    
    public static final int    MASTER_PORTINFO_DEFAULT  = 60010;
    
    
    public static final String REGION_PORT_PARAM        = "hbase.regionserver.port";
    
    public static final int    REGION_PORT_DEFAULT      = 60020;    
    
    public static final String REGION_PORTINFO_PARAM    = "hbase.regionserver.info.port";
    
    public static final int    REGION_PORTINFO_DEFAULT  = 60030;
    
    
    public static final String REST_PORT_PARAM          = "hbase.rest.port**";
    
    public static final int    REST_PORT_DEFAULT        = 8080;    
    
    public static final String REST_PORTINFO_PARAM      = " hbase.rest.info.port*";
    
    public static final int    REST_PORTINFO_DEFAULT    = 8085;
    
    public static final String THRIFT_PORT_PARAM        = "hbase.thrift.port**";
    
    public static final int    THRIFT_PORT_DEFAULT      = 9090;    
    
    public static final String THRIFT_PORTINFO_PARAM    = "hbase.thrift.info.port**";
    
    public static final int    THRIFT_PORTINFO_DEFAULT  = 9095;
    
    // -----  Features ---------
    
    /** column Family. */
    public static final String FEATURES_TABLENAME_ID = "ff4j_features";
    
    /** column Family. */
    public static final TableName FEATURES_TABLENAME = TableName.valueOf(FEATURES_TABLENAME_ID);
    
    /** column Family. */
    public static final String FEATURES_CF_CORE = "f";
    
    /** column Family. */
    public static final byte[] B_FEATURES_CF_CORE = Bytes.toBytes(FEATURES_CF_CORE);

    /** column Family. */
    public static final String FEATURES_CF_PROPERTIES = "p";
    
    /** column Family. */
    public static final byte[] B_FEATURES_CF_PROPERTIES = Bytes.toBytes(FEATURES_CF_PROPERTIES);
    
    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_UID = "UID";
    
    /** column Family. */
    public static final byte[] B_FEAT_UID = Bytes.toBytes(COL_FEAT_UID);

    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_ENABLE = "ENABLE";
    
    /** column Family. */
    public static final byte[] B_FEAT_ENABLE = Bytes.toBytes(COL_FEAT_ENABLE);

    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_DESCRIPTION = "DESCRIPTION";
    
    /** column Family. */
    public static final byte[] B_FEAT_DESCRIPTION = Bytes.toBytes(COL_FEAT_DESCRIPTION);

    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_GROUPNAME = "GROUPNAME";

    /** column Family. */
    public static final byte[] B_FEAT_GROUPNAME = Bytes.toBytes(COL_FEAT_GROUPNAME);
    
    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_STRATEGY = "STRATEGY";

    /** column Family. */
    public static final byte[] B_FEAT_STRATEGY = Bytes.toBytes(COL_FEAT_STRATEGY);
    
    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_EXPRESSION = "EXPRESSION";
    
    /** column Family. */
    public static final byte[] B_FEAT_EXPRESSION = Bytes.toBytes(COL_FEAT_EXPRESSION);
    
    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_ROLES = "ROLES";
    
    /** column Family. */
    public static final byte[] B_FEAT_ROLES = Bytes.toBytes(COL_FEAT_ROLES);
    
    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_CUSTOMPROPERTIES = "PROPERTIES";
    
    /** column Family. */
    public static final byte[] B_FEAT_CUSTOMPROPERTIES = Bytes.toBytes(COL_FEAT_CUSTOMPROPERTIES);
    
    
    // ----- Property ------------------
    
    /** column Family. */
    public static final String PROPERTIES_TABLENAME_ID = "ff4j_properties";
    
    /** column Family. */
    public static final TableName PROPERTIES_TABLENAME = TableName.valueOf(PROPERTIES_TABLENAME_ID);
    
    /** sql column name from table FF4J_PROPERTIES. */
    public static final String COL_PROPERTY_ID = "UID";
    
    /** column Family. */
    public static final byte[] B_COL_PROPERTY_ID = Bytes.toBytes(COL_PROPERTY_ID);
    
    /** sql column name from table FF4J_PROPERTIES. */
    public static final String COL_PROPERTY_CLAZZ = "CLAZZ";
    
    /** column Family. */
    public static final byte[] B_COL_PROPERTY_CLAZZ = Bytes.toBytes(COL_PROPERTY_CLAZZ);
    
    /** sql column name from table FF4J_PROPERTIES. */
    public static final String COL_PROPERTY_VALUE = "VALUE";
    
    /** column Family. */
    public static final byte[] B_COL_PROPERTY_VALUE = Bytes.toBytes(COL_PROPERTY_VALUE);
    
    /** sql column name from table FF4J_PROPERTIES. */
    public static final String COL_PROPERTY_FIXED = "FIXEDVALUES";
    
    /** column Family. */
    public static final byte[] B_COL_PROPERTY_FIXED = Bytes.toBytes(COL_PROPERTY_FIXED);
    
    /** sql column name from table FF4J_PROPERTIES. */
    public static final String COL_PROPERTY_FEATID = "FEAT_UID";
    
    /** column Family. */
    public static final byte[] B_COL_PROPERTY_FEATID = Bytes.toBytes(COL_PROPERTY_FEATID);
    
    /** sql column name from table FF4J_PROPERTIES. */
    public static final String COL_PROPERTY_DESCRIPTION = "DESCRIPTION";
    
    /** column Family. */
    public static final byte[] B_COL_PROPERTY_DESCRIPTION = Bytes.toBytes(COL_PROPERTY_DESCRIPTION);
    
    // --- AUDIT -----
    
    /** column Family. */
    public static final String    AUDIT_TABLENAME_ID = "ff4j_audit";
    public static final TableName AUDIT_TABLENAME    = TableName.valueOf(AUDIT_TABLENAME_ID);
    
    /** column Family. */
    public static final String    AUDIT_CF = "evt";
    public static final byte[]    B_AUDIT_CF         = Bytes.toBytes(AUDIT_CF);
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_UID    = "UID";
    public static final byte[] B_EVENT_UID  = Bytes.toBytes(COL_EVENT_UID);

    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_TIME       = "TIME";
    public static final byte[] B_EVENT_TIME     = Bytes.toBytes(COL_EVENT_TIME);
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_DATE       = "DATE";
    public static final byte[] B_EVENT_DATE     = Bytes.toBytes(COL_EVENT_DATE);
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_TYPE       = "TYPE";
    public static final byte[] B_EVENT_TYPE     = Bytes.toBytes(COL_EVENT_TYPE);
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_NAME       = "NAME";
    public static final byte[] B_EVENT_NAME     = Bytes.toBytes(COL_EVENT_NAME);
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_ACTION     = "ACTION";
    public static final byte[] B_EVENT_ACTION   = Bytes.toBytes(COL_EVENT_ACTION);
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_HOSTNAME   = "HOSTNAME";
    public static final byte[] B_EVENT_HOSTNAME = Bytes.toBytes(COL_EVENT_HOSTNAME);
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_SOURCE     = "SOURCE";
    public static final byte[] B_EVENT_SOURCE   = Bytes.toBytes(COL_EVENT_SOURCE);
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_DURATION   = "DURATION";
    public static final byte[] B_EVENT_DURATION = Bytes.toBytes(COL_EVENT_DURATION);
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_USER       = "USER";
    public static final byte[] B_EVENT_USER     = Bytes.toBytes(COL_EVENT_USER);
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_VALUE      = "VALUE";
    public static final byte[] B_EVENT_VALUE    = Bytes.toBytes(COL_EVENT_VALUE);
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_KEYS       = "KEYS";
    public static final byte[] B_EVENT_KEYS     = Bytes.toBytes(COL_EVENT_KEYS);
    
    /** Group of all columns ('*' does not exist). */
    public static final Set < String > COLS_EVENT = Util.set(COL_EVENT_ACTION, COL_EVENT_DATE, COL_EVENT_DURATION,
            COL_EVENT_HOSTNAME, COL_EVENT_KEYS, COL_EVENT_NAME, COL_EVENT_SOURCE, COL_EVENT_TIME,
            COL_EVENT_TYPE, COL_EVENT_UID, COL_EVENT_USER, COL_EVENT_VALUE);
    
    /** Remove public constructor. */
    private HBaseConstants() {
    }
}
