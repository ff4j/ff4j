package org.ff4j.couchbase.store;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.ff4j.couchbase.CouchbaseConnection;
import org.ff4j.couchbase.mapper.PropertyCouchbaseMapper;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.utils.Util;
import org.ff4j.utils.json.PropertyJsonParser;

/*
 * #%L
 * ff4j-store-springcouchbase
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

import com.couchbase.client.java.Bucket;
import com.couchbase.client.java.query.N1qlQuery;
import com.couchbase.client.java.query.N1qlQueryResult;
import com.couchbase.client.java.query.N1qlQueryRow;

/**
 * Created by farrellyja on 10/11/2017.
 */
public class PropertyStoreCouchbase extends AbstractPropertyStore {

    /** Couchebase mapper. */
    private PropertyCouchbaseMapper PROPERTY_MAPPER = new PropertyCouchbaseMapper();
    
    /** Keep reference to connection. */
    private CouchbaseConnection couchBaseConnection;
    
    /** Keep reference to bucket. */
    private Bucket propertyBucket;
    
    /**
     * Default initialisation
     */
    public PropertyStoreCouchbase() {}
    
    /**
     * Initialization thourhg connection
     * @param conn
     */
    public PropertyStoreCouchbase(CouchbaseConnection conn) {
        this.couchBaseConnection = conn;
    }
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        throw new UnsupportedOperationException("Cannot create buckets from Java driver");
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean existProperty(String name) {
        Util.assertHasLength(name);
        return getPropertyBucket().exists(name);
    }

    /** {@inheritDoc} */
    @Override
    public <T> void createProperty(Property<T> prop) {
        assertPropertyNotNull(prop);
        assertPropertyNotExist(prop.getName());
        if (prop.getFixedValues() != null && !prop.getFixedValues().isEmpty() && !prop.getFixedValues().contains(prop.getValue())) {
            throw new IllegalArgumentException("Value " + prop.getValue() + " is not within fixed values " + prop.getFixedValues());
        }
        getPropertyBucket().upsert(PROPERTY_MAPPER.toStore(prop));
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> readProperty(String name) {
        assertPropertyExist(name);
        return PROPERTY_MAPPER.fromStore(getPropertyBucket().get(name));
    }

    /** {@inheritDoc} */
    @Override
    public void deleteProperty(String name) {
        assertPropertyExist(name);
        getPropertyBucket().remove(name);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Property<?>> readAllProperties() {
        N1qlQuery queryFeatures = N1qlQuery.simple("SELECT * FROM " + couchBaseConnection.getFf4jPropertyBucketName());
        N1qlQueryResult queryResult = getPropertyBucket().query(queryFeatures);
        Map<String, Property<?>> allProperties = new HashMap<>();
        for (N1qlQueryRow row : queryResult.allRows()) {
            Property<?> p = PropertyJsonParser.parseProperty(row.value().get(couchBaseConnection.getFf4jPropertyBucketName()).toString());
            allProperties.put(p.getName(), p);
        }
        return allProperties;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> listPropertyNames() {
        return readAllProperties().keySet();
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        getPropertyBucket().bucketManager().flush();
    }
    
    /**
     * Access to feature bucket.
     *
     * @return
     *      reference to bucket
     */
    private Bucket getPropertyBucket() {
        if (propertyBucket == null) {
            Util.assertNotNull(getCouchBaseConnection());
            propertyBucket = getCouchBaseConnection().getPropertiesBucket();
            Util.assertNotNull(propertyBucket);
        }
        return propertyBucket;
    }

    /**
     * Getter accessor for attribute 'couchBaseConnection'.
     *
     * @return
     *       current value of 'couchBaseConnection'
     */
    public CouchbaseConnection getCouchBaseConnection() {
        return couchBaseConnection;
    }

    /**
     * Setter accessor for attribute 'couchBaseConnection'.
     * @param couchBaseConnection
     * 		new value for 'couchBaseConnection '
     */
    public void setCouchBaseConnection(CouchbaseConnection couchBaseConnection) {
        this.couchBaseConnection = couchBaseConnection;
    }
}
