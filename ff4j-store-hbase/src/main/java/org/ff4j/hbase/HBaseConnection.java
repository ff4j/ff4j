package org.ff4j.hbase;

/*
 * #%L
 * ff4j-store-hbase
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import java.io.IOException;
import java.util.Arrays;
import java.util.Set;
import java.util.stream.Stream;

import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.hbase.HBaseConfiguration;
import org.apache.hadoop.hbase.HColumnDescriptor;
import org.apache.hadoop.hbase.HTableDescriptor;
import org.apache.hadoop.hbase.TableName;
import org.apache.hadoop.hbase.client.Admin;
import org.apache.hadoop.hbase.client.Connection;
import org.apache.hadoop.hbase.client.ConnectionFactory;
import org.apache.hadoop.hbase.client.HBaseAdmin;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Connection to Cassandra.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class HBaseConnection {
    
    /** logger for this store. */
    private static Logger LOGGER = LoggerFactory.getLogger(HBaseConnection.class);
    
    /** Hbase configuration. */
    private Configuration config = null;
    
    /**
     * Default Settings.
     */
    public HBaseConnection() {
        this(true);
    }
    
    /**
     * Create connection from XML File.
     *
     * @param xmlConf
     *      target xml config
     * @param checkConfig
     *      if the connection must be checked
     */
    public HBaseConnection(String xmlConf, boolean checkConfig) {
        config = new Configuration(true);
        config.addResource(new Path(xmlConf));
        if (checkConfig) checkHBaseConfiguration();
    }
        
    /**
     * Defaut settings but can skip validation.
     *
     * @param checkConfig
     *      flag to avoid validate configuration
     */
    public HBaseConnection(boolean checkConfig) {
        this(HBaseConstants.ZOOKEEPER_QUORUM_DEFAULT, HBaseConstants.ZOOKEEPER_CLIENTPORT_DEFAULT, checkConfig);
    }
    
    /**
     * Constructor with parameters.
     *
     * @param zooKeeperQuorum
     *      list of zookeeper host separater by a comma
     * @param zooKeeperClientPort
     *      zookeeper port (should be the same for all?)
     * @param checkConfig
     */
    public HBaseConnection(String zooKeeperQuorum, int zooKeeperClientPort, boolean checkConfig) {
        config = HBaseConfiguration.create();
        config.clear(); 
        config.set(HBaseConstants.ZOOKEEPER_QUORUM_PARAM, zooKeeperQuorum); 
        config.setInt(HBaseConstants.ZOOKEEPER_CLIENTPORT_PARAM, zooKeeperClientPort);
        if (checkConfig) checkHBaseConfiguration();
    }
    
    /** Default. */
    public HBaseConnection(Configuration hbaseConf, boolean checkConfig) {
        this.config = hbaseConf;
        if (checkConfig) checkHBaseConfiguration();
    }
    
    /**
     * Allows check the configuration
     */
    public void checkHBaseConfiguration() {
        if (config == null) {
            throw new IllegalStateException("Connection has not been initialized");
        }
        try {
            HBaseAdmin.checkHBaseAvailable(config);
        } catch (Exception e) {
           throw new IllegalArgumentException("Cannot connect to server HBASE, please check /etc/hosts or settings.");
        } 
    }
    
    /**
     * Helper to create a table in HBASE.
     *
     * @param tableName
     *      current table name
     * @param columnFamilies
     *      list of column families
     */
    public void createTable(String tableName, Set<String> columnFamilies) {
        try (Connection hbConn = ConnectionFactory.createConnection(config)) {
            try(Admin hbAdmin = hbConn.getAdmin()) {
                TableName hTableName = TableName.valueOf(tableName);
                if (!hbAdmin.tableExists(hTableName)) {
                    HTableDescriptor tableDesc = new HTableDescriptor(hTableName);
                    columnFamilies.stream().map(HColumnDescriptor::new).forEach(tableDesc::addFamily);
                    hbAdmin.createTable(tableDesc);
                } else {
                    LOGGER.info("Table " + tableName + " already exists.");
                }
            }
        } catch (IOException e) {
            throw new IllegalArgumentException("Cannot create table " + tableName + " please check name", e);
        }
    }
    
    /**
     * Clear a table by its name.
     * 
     * @param tableName
     *      table name
     */
    public void truncateTable(String tableName) {
        Connection hbConn = null;
        Admin hbAdmin     = null;
        try {
            hbConn = ConnectionFactory.createConnection(config);
            hbAdmin = hbConn.getAdmin();
            TableName target = TableName.valueOf(tableName);
            hbAdmin.disableTable(target);
            hbAdmin.truncateTable(target, false);
        } catch (IOException e) {
            throw new IllegalArgumentException("Cannot truncate table " + tableName + " please check name", e);
        } finally {
            // Closing connection
            try {
                if (hbAdmin != null) hbAdmin.close();
                if (hbConn != null)  hbConn.close();
            } catch (IOException e) {}
        }
    }
    
    /**
     * List tables in DB.
     *
     * @return
     *      get table
     * @throws IOException
     */
    public Stream<String> listTablesName() throws IOException {
        try (Connection hbConn = ConnectionFactory.createConnection(config)) {
            try(Admin hbAdmin = hbConn.getAdmin()) {
                return Arrays.stream(hbAdmin.listTables()).map(HTableDescriptor::getNameAsString);
            }
        } catch (IOException e) {
            throw new IllegalArgumentException("Cannot list table names", e);
        }
    }

    /**
     * Getter accessor for attribute 'config'.
     *
     * @return
     *       current value of 'config'
     */
    public Configuration getConfig() {
        return config;
    }
}
