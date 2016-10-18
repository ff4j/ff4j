package org.ff4j.cassandra.store;

import static org.ff4j.cassandra.CassandraConstants.COLUMN_FAMILY_PROPERTIES;
import static org.ff4j.cassandra.CassandraConstants.COL_PROPERTY_ID;

import java.util.HashMap;
import java.util.HashSet;

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


import java.util.Map;
import java.util.Set;

import org.ff4j.cassandra.CassandraConnection;
import org.ff4j.cassandra.CassandraMapper;
import org.ff4j.cassandra.CassandraQueryBuilder;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.utils.Util;

import com.datastax.driver.core.ResultSet;
import com.datastax.driver.core.Row;

/**
 * Implements of {@link PropertyStore} for sotre Cassandra.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class PropertyStoreCassandra extends AbstractPropertyStore {
    
    /** Connection to store Cassandra. */
    private CassandraQueryBuilder builder;
            
    /** Connection to store Cassandra. */
    private CassandraConnection conn;
    
    /**
     * Default constructor.
     */
    public PropertyStoreCassandra() {
    }
    
    /**
     * Initialization through {@link CassandraConnection}.
     *
     * @param conn
     *      current client to cassandra db
     */
    public PropertyStoreCassandra(CassandraConnection conn) {
        this.conn = conn;
    }

    /** {@inheritDoc} */
    @Override
    public void createSchema() {
       // Roles & custom properties will be in the same column family  
       if (!conn.isColumnFamilyExist(COLUMN_FAMILY_PROPERTIES)) {
           // Create table
           conn.getSession().execute(getBuilder().cqlCreateColumnFamilyProperties());
       }
    }

    /** {@inheritDoc} */
    @Override
    public boolean existProperty(String name) {
        Util.assertHasLength(name);
        return 1 == conn.getSession()
                .execute(getBuilder().cqlExistProperty(), name)
                .iterator().next().getLong(0);
    }

    /** {@inheritDoc} */
    @Override
    public <T> void createProperty(Property<T> prop) {
        assertPropertyNotNull(prop);
        assertPropertyNotExist(prop.getName());
        Set < String > fixedValues = new HashSet<String>();
        if (prop.getFixedValues() != null) {
            for (T fixedValue : prop.getFixedValues()) {
                fixedValues.add(fixedValue.toString());
            }
        }
        conn.getSession().execute(getBuilder().cqlCreateProperty(), 
                prop.getName(),
                prop.getType(),
                prop.asString(),
                prop.getDescription(),
                fixedValues);
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> readProperty(String name) {
        assertPropertyExist(name);
        ResultSet rs = conn.getSession().execute(getBuilder().cqlReadProperty(), name);
        return CassandraMapper.mapProperty(rs.one());
    }

    /** {@inheritDoc} */
    @Override
    public void deleteProperty(String name) {
        assertPropertyExist(name);
        conn.getSession().execute(getBuilder().cqlDeleteProperty(), name);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Property<?>> readAllProperties() {
        Map < String, Property<?>> properties = new HashMap<String, Property<?>>();
        ResultSet resultSet = conn.getSession().execute(getBuilder().selectAllProperties());
        for (Row row : resultSet.all()) {
            Property<?> p  = CassandraMapper.mapProperty(row);
            properties.put(p.getName(), p);
        }
        return properties;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> listPropertyNames() {
        Set < String > listProperty = new HashSet<String>();
        ResultSet resultSet = conn.getSession().execute(getBuilder().cqlPropertyNames());
        for (Row row : resultSet.all()) {
            listProperty.add(row.getString(COL_PROPERTY_ID));
        }
        return listProperty;
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        conn.getSession().execute(getBuilder().cqlTruncateProperties());
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
     *      new value for 'conn '
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
