package org.ff4j.couchbase.store;

/*-
 * #%L
 * ff4j-store-couchbase
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
import java.util.Map;
import java.util.Set;

import org.ff4j.couchbase.CouchbaseConnection;
import org.ff4j.couchbase.mapper.PropertyCouchbaseMapper;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.utils.Util;
import org.ff4j.utils.json.PropertyJsonParser;

import com.couchbase.client.java.Collection;
import com.couchbase.client.java.json.JsonObject;
import com.couchbase.client.java.query.QueryResult;

/**
 * Created by farrellyja on 10/11/2017.
 */
public class PropertyStoreCouchbase extends AbstractPropertyStore {

    /** Couchebase mapper. */
    private PropertyCouchbaseMapper PROPERTY_MAPPER = new PropertyCouchbaseMapper();
    
    /** Keep reference to connection. */
    private CouchbaseConnection couchBaseConnection;
    
    /** Keep reference to bucket. */
    private Collection propertyCollection;
    
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
        return getPropertyCollection().exists(name).exists();
    }

    /** {@inheritDoc} */
    @Override
    public <T> void createProperty(Property<T> prop) {
        assertPropertyNotNull(prop);
        assertPropertyNotExist(prop.getName());
        if (prop.getFixedValues() != null && !prop.getFixedValues().isEmpty() && !prop.getFixedValues().contains(prop.getValue())) {
            throw new IllegalArgumentException("Value " + prop.getValue() + " is not within fixed values " + prop.getFixedValues());
        }
        getPropertyCollection().upsert(prop.getName(), PROPERTY_MAPPER.toStore(prop));
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> readProperty(String name) {
        assertPropertyExist(name);
        return PROPERTY_MAPPER.fromStore(getPropertyCollection().get(name).contentAsObject());
    }

    /** {@inheritDoc} */
    @Override
    public void deleteProperty(String name) {
        assertPropertyExist(name);
        getPropertyCollection().remove(name);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Property<?>> readAllProperties() {
        String bucketName = couchBaseConnection.getFf4jPropertyBucketName();
        QueryResult queryResult = couchBaseConnection.getCluster()
                .query("SELECT RAW property FROM `" + bucketName.replace("`", "``") + "` AS property");
        Map<String, Property<?>> allProperties = new HashMap<>();
        for (JsonObject row : queryResult.rowsAsObject()) {
            Property<?> p = PropertyJsonParser.parseProperty(row.toString());
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
        couchBaseConnection.getCluster().buckets().flushBucket(couchBaseConnection.getFf4jPropertyBucketName());
    }
    
    /**
     * Access to feature bucket.
     *
     * @return
     *      reference to bucket
     */
    private Collection getPropertyCollection() {
        if (propertyCollection == null) {
            Util.assertNotNull(getCouchBaseConnection());
            propertyCollection = getCouchBaseConnection().getPropertiesBucket().defaultCollection();
            Util.assertNotNull(propertyCollection);
        }
        return propertyCollection;
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
