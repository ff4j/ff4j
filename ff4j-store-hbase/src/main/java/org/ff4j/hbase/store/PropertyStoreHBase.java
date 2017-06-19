package org.ff4j.hbase.store;

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


import static org.ff4j.hbase.HBaseConstants.B_FEATURES_CF_PROPERTIES;
import static org.ff4j.hbase.HBaseConstants.FEATURES_CF_PROPERTIES;
import static org.ff4j.hbase.HBaseConstants.PROPERTIES_TABLENAME;
import static org.ff4j.hbase.HBaseConstants.PROPERTIES_TABLENAME_ID;

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
import org.apache.hadoop.hbase.client.Get;
import org.apache.hadoop.hbase.client.Put;
import org.apache.hadoop.hbase.client.Result;
import org.apache.hadoop.hbase.client.ResultScanner;
import org.apache.hadoop.hbase.client.Scan;
import org.apache.hadoop.hbase.client.Table;
import org.apache.hadoop.hbase.util.Bytes;
import org.ff4j.exception.PropertyAccessException;
import org.ff4j.hbase.HBaseConnection;
import org.ff4j.hbase.mapper.HBasePropertyMapper;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.utils.Util;

/**
 * Implements of {@link PropertyStore} for Store Hbase
 *
 * @author Cedrick Lunven (@clunven)
 */
public class PropertyStoreHBase extends AbstractPropertyStore {
    
    /** Mapper. */
    private static final HBasePropertyMapper MAPPER = new HBasePropertyMapper();
    
    /** Connection to store Cassandra. */
    private HBaseConnection conn;
    
    /**
     * Default constructor.
     */
    public PropertyStoreHBase() {
    }
    
    /**
     * Initialization through {@link HBaseConnection}.
     *
     * @param conn
     *      current client to cassandra db
     */
    public PropertyStoreHBase(HBaseConnection conn) {
        this.conn = conn;
    }    

    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        conn.createTable(PROPERTIES_TABLENAME_ID, Util.set(FEATURES_CF_PROPERTIES));
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean existProperty(String name) {
        Util.assertHasLength(name);
        try (Connection hbConn = ConnectionFactory.createConnection(conn.getConfig())) {
            try(Table table = hbConn.getTable(PROPERTIES_TABLENAME)) {
                return !table.get(new Get(Bytes.toBytes(name))).isEmpty();
            }
        } catch (IOException e) {
            throw new PropertyAccessException("Cannot check property existence", e);
        }
    }

    /** {@inheritDoc} */
    @Override
    public <T> void createProperty(Property<T> prop) {
        assertPropertyNotNull(prop);
        assertPropertyNotExist(prop.getName());
        executePutCommand(MAPPER.toStore(prop));
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> readProperty(String name) {
        assertPropertyExist(name);
        try (Connection hbConn = ConnectionFactory.createConnection(conn.getConfig())) {
            try(Table table = hbConn.getTable(PROPERTIES_TABLENAME)) {
                Result result = table.get(new Get(Bytes.toBytes(name)));
                return MAPPER.fromStore(result);
            }
        } catch (IOException e) {
            throw new PropertyAccessException("Cannot read property", e);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void deleteProperty(String name) {
        assertPropertyExist(name);
        try (Connection hbConn = ConnectionFactory.createConnection(conn.getConfig())) {
            try(Table table = hbConn.getTable(PROPERTIES_TABLENAME)) {
                List<Delete> list = new ArrayList<Delete>();
                Delete del = new Delete(name.getBytes());
                list.add(del);
                table.delete(list);
            }
        } catch (IOException e) {
            throw new PropertyAccessException("Cannot delete property ", e);
        }
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Property<?>> readAllProperties() {
        Map<String, Property<?>> mapOfProperty = new HashMap<>();
        try (Connection hbConn = ConnectionFactory.createConnection(conn.getConfig())) {
            try(Table table = hbConn.getTable(PROPERTIES_TABLENAME)) {
                
                Scan scan = new Scan();
                scan.setCaching(100);
                scan.setBatch(100);
                scan.addFamily(B_FEATURES_CF_PROPERTIES);
                
                try(ResultScanner resultScanner = table.getScanner(scan)) {
                    Iterator<Result> iterator = resultScanner.iterator();
                    while (iterator.hasNext()) {
                        Property<?> p = MAPPER.fromStore(iterator.next());
                        mapOfProperty.put(p.getName(), p);
                    }
                }
            }
        } catch (IOException e) {
            throw new PropertyAccessException("Cannot read all property", e);
        }
        return mapOfProperty;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> listPropertyNames() {
        return readAllProperties().keySet();
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        conn.truncateTable(PROPERTIES_TABLENAME_ID);
    }
    
    /**
     * PUT query.
     *
     * @param putQuery
     *      query to insert into data
     */
    private void executePutCommand(Put putQuery) {
        try (Connection hbConn = ConnectionFactory.createConnection(conn.getConfig())) {
            try(Table table = hbConn.getTable(PROPERTIES_TABLENAME)) {
                table.put(putQuery);
            }
        } catch (IOException e) {
            throw new PropertyAccessException("Cannot execute command", e);
        }
    }
    
}
