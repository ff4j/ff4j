package org.ff4j.cassandra.store;

/*-
 * #%L
 * ff4j-store-cassandra
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import java.util.HashMap;
import java.util.HashSet;

import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.ff4j.cassandra.FF4jCassandraSchema;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.property.util.PropertyFactory;
import org.ff4j.utils.Util;

import com.datastax.oss.driver.api.core.CqlSession;
import com.datastax.oss.driver.api.core.cql.BoundStatement;
import com.datastax.oss.driver.api.core.cql.PreparedStatement;
import com.datastax.oss.driver.api.core.cql.ResultSet;
import com.datastax.oss.driver.api.core.cql.Row;
import com.datastax.oss.driver.api.querybuilder.QueryBuilder;

/**
 * Implements of {@link PropertyStore} for store Cassandra.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class PropertyStoreCassandra extends AbstractPropertyStore implements FF4jCassandraSchema {
    
    /** Driver Session. */
    private final CqlSession cqlSession;
    
    /** Statements. */
    private PreparedStatement psExistProperty;
    private PreparedStatement psInsertProperty;
    private PreparedStatement psReadProperty; 
    private PreparedStatement psDeleteProperty;

    /**
     * Connector with running session
     */
    public PropertyStoreCassandra(CqlSession cqlSession) {
        this.cqlSession = cqlSession;
    }

    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        cqlSession.execute(STMT_CREATE_TABLE_PROPERTY);
    }

    /** {@inheritDoc} */
    @Override
    public boolean existProperty(String name) {
        Util.assertHasLength(name);
        return getCqlSession().execute(psExistProperty.bind(name))
                .getAvailableWithoutFetching() > 0;
    }

    /** {@inheritDoc} */
    @Override
    public <T> void createProperty(Property<T> prop) {
        assertPropertyNotNull(prop);
        assertPropertyNotExist(prop.getName());
        Set < String > fixedValues = new HashSet<>();
        if (prop.getFixedValues() != null) {
            for (T fixedValue : prop.getFixedValues()) {
                fixedValues.add(fixedValue.toString());
            }
        }
        BoundStatement bsInsertProperty = psInsertProperty.bind();
        bsInsertProperty = bsInsertProperty.setString(PROPERTIES_ATT_UID, prop.getName());
        bsInsertProperty = bsInsertProperty.setString(PROPERTIES_ATT_CLASS, prop.getClass().getName());
        bsInsertProperty = bsInsertProperty.setString(PROPERTIES_ATT_VALUE, prop.asString());
        bsInsertProperty = bsInsertProperty.setString(PROPERTIES_ATT_DESCRIPTION, prop.getDescription());
        bsInsertProperty = bsInsertProperty.setSet(PROPERTIES_ATT_FIXEDVALUES, fixedValues, String.class);
        cqlSession.execute(bsInsertProperty);
    }
    

    /** {@inheritDoc} */
    @Override
    public Property<?> readProperty(String name) {
        assertPropertyExist(name);
        ResultSet rs = cqlSession.execute(psReadProperty.bind(name));
        Row row = rs.one();
        if (null == row) {
            throw new PropertyNotFoundException(name);
        }
        return mapPropertyRow(row);
    }

    /** {@inheritDoc} */
    @Override
    public void deleteProperty(String name) {
        assertPropertyExist(name);
        cqlSession.execute(psDeleteProperty.bind(name));
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Property<?>> readAllProperties() {
        Map < String, Property<?>> properties = new HashMap<>();
        ResultSet rs = cqlSession.execute(STMT_PROPERTY_READ_ALL);
        for (Row row : rs.all()) {
            Property<?> p  = mapPropertyRow(row);
            properties.put(p.getName(), p);
        }
        return properties;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> listPropertyNames() {
        return cqlSession.execute(STMT_PROPERTY_LISTNAMES)
                .all().stream()
                .map(r -> r.getString(PROPERTIES_ATT_UID))
                .collect(Collectors.toSet());
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        cqlSession.execute(QueryBuilder.truncate(PROPERTIES_TABLE).build());
    }
    
    /**
     * Prepared once, run many.
     */
    protected void prepareStatements() {
        psExistProperty  = cqlSession.prepare(STMT_PROPERTY_EXIST);
        psInsertProperty = cqlSession.prepare(STMT_PROPERTY_INSERT);
        psReadProperty   = cqlSession.prepare(STMT_PROPERTY_READ);
        psDeleteProperty = cqlSession.prepare(STMT_PROPERTY_DELETE);
    }
    
    /**
     * Map from a row to property.
     */
    protected Property<?> mapPropertyRow(Row row) {
        String propName  = row.getString(PROPERTIES_ATT_UID);
        String propClass = row.getString(PROPERTIES_ATT_CLASS);
        String propDesc  = row.getString(PROPERTIES_ATT_DESCRIPTION);
        String propVal   = row.getString(PROPERTIES_ATT_VALUE);
        Set<String> fixV = row.getSet(PROPERTIES_ATT_FIXEDVALUES, String.class);
        return PropertyFactory.createProperty(propName, propClass, propVal, propDesc, fixV);
    }
    
    /**
     * Prepared statements on first call.
     */
    private synchronized CqlSession getCqlSession() {
        if (null == psExistProperty) {
            prepareStatements();
        }
        return cqlSession;
    }
    
}
