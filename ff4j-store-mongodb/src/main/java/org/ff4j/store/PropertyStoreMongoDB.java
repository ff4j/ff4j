package org.ff4j.store;

import static org.ff4j.store.mongodb.FeatureStoreMongoConstants.MONGO_SET;
import static org.ff4j.store.mongodb.FeatureStoreMongoConstants.PROPERTY_NAME;

/*
 * #%L
 * ff4j-store-mongodb
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


import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.store.mongodb.FeatureDBObjectMapper;
import org.ff4j.store.mongodb.PropertyDBObjectBuilder;
import org.ff4j.utils.Util;

import com.mongodb.BasicDBObject;
import com.mongodb.BasicDBObjectBuilder;
import com.mongodb.DBCollection;
import com.mongodb.DBObject;

/**
 * PropertyStore based on MongoDB database.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class PropertyStoreMongoDB extends AbstractPropertyStore {

    /** MongoDB collection. */
    private final DBCollection collection;
    
    /** Property mapper. */
    private FeatureDBObjectMapper MAPPER = new FeatureDBObjectMapper();
    
    /** Build fields. */
    private static final PropertyDBObjectBuilder BUILDER = new PropertyDBObjectBuilder();
    
    /**
     * Parameterized constructor with collection.
     * 
     * @param collection
     *            the collection to set
     */
    public PropertyStoreMongoDB(DBCollection collection) {
        this.collection = collection;
    }
    
    /**
     * Parameterized constructor with collection.
     * 
     * @param collection
     *            the collection to set
     */
    public PropertyStoreMongoDB(DBCollection collection, String xmlConfFile) {
        this(collection);
        importPropertiesFromXmlFile(xmlConfFile);
    }
    
    /** {@inheritDoc} */
    public boolean existProperty(String name) {
        Util.assertHasLength(name);
        return 1 == collection.count(BUILDER.getName(name));
    }

    /** {@inheritDoc} */
    public <T> void createProperty(Property<T> prop) {
        if (prop == null) {
            throw new IllegalArgumentException("Property cannot be null nor empty");
        }
        if (existProperty(prop.getName())) {
            throw new PropertyAlreadyExistException(prop.getName());
        }
        collection.save(MAPPER.fromProperty2DBObject(prop));
    }

    /** {@inheritDoc} */
    public Property<?> readProperty(String name) {
        assertPropertyName(name);
        DBObject object = collection.findOne(BUILDER.getName(name));
        return MAPPER.mapProperty(object);
    }
    
    /** {@inheritDoc} */
    public void deleteProperty(String name) {
        assertPropertyName(name);
        collection.remove(BUILDER.getName(name));
    }
    
    /** {@inheritDoc} */
    public void clear() {
        collection.remove(BasicDBObjectBuilder.start().get());
    }

    /** {@inheritDoc} */
    public void updateProperty(String name, String newValue) {
        assertPropertyName(name);
        readProperty(name).fromString(newValue);
        DBObject query = BUILDER.getName(name);
        Object update = BUILDER.getValue(newValue);
        collection.update(query, BasicDBObjectBuilder.start(MONGO_SET, update).get());
    }

    /** {@inheritDoc} */
    public <T> void updateProperty(Property<T> prop) {
        Util.assertNotNull(prop);
        // Delete
        deleteProperty(prop.getName());
        // Create
        createProperty(prop);
    }
    
    /** {@inheritDoc} */
    public Map<String, Property<?>> readAllProperties() {
        LinkedHashMap<String, Property<?>> mapP = new LinkedHashMap<String, Property<?>>();
        for(DBObject dbObject : collection.find()) {
            Property<?> prop = MAPPER.mapProperty(dbObject);
            mapP.put(prop.getName(), prop);
        }
        return mapP;
    }

    /** {@inheritDoc} */
    public Set<String> listPropertyNames() {
        Set < String > properties = new HashSet<String>();
        for(DBObject dbObject : collection.find(
                new BasicDBObject(), 
                new BasicDBObjectBuilder().add(PROPERTY_NAME, true).get())) {
            properties.add((String) dbObject.get(PROPERTY_NAME));
        }
        return properties;
    }

}
