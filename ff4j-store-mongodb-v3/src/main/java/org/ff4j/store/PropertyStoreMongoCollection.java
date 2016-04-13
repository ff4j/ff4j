package org.ff4j.store;

/*
 * #%L
 * ff4j-store-mongodb-v3
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

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.bson.Document;
import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.store.mongodb.FeatureDocumentMapper;
import org.ff4j.store.mongodb.PropertyDocumentBuilder;
import org.ff4j.utils.Util;

import com.mongodb.client.MongoCollection;

import static org.ff4j.store.mongodb.FeatureStoreMongoConstants.*;

/**
 * PropertyStore based on MongoDB database.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class PropertyStoreMongoCollection extends AbstractPropertyStore {

    /** MongoDB collection. */
    private final MongoCollection<Document> collection;
    
    /** Property mapper. */
    private FeatureDocumentMapper MAPPER = new FeatureDocumentMapper();
    
    /** Build fields. */
    private static final PropertyDocumentBuilder BUILDER = new PropertyDocumentBuilder();
    
    /**
     * Parameterized constructor with collection.
     * 
     * @param collection
     *            the collection to set
     */
    public PropertyStoreMongoCollection(MongoCollection<Document> collection) {
        this.collection = collection;
    }
    
    /**
     * Parameterized constructor with collection.
     * 
     * @param collection
     *            the collection to set
     */
    public PropertyStoreMongoCollection(MongoCollection<Document> collection, String xmlConfFile) {
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
        collection.insertOne(MAPPER.fromProperty2DBObject(prop));
    }

    /** {@inheritDoc} */
    public Property<?> readProperty(String name) {
        assertPropertyName(name);
        Document object = collection.find(BUILDER.getName(name)).first();
        return MAPPER.mapProperty(object);
    }
    
    /** {@inheritDoc} */
    public void deleteProperty(String name) {
        assertPropertyName(name);
        collection.deleteOne(BUILDER.getName(name));
    }
    
    /** {@inheritDoc} */
    public void clear() {
        collection.deleteMany(new Document());
    }

    /** {@inheritDoc} */
    public void updateProperty(String name, String newValue) {
        assertPropertyName(name);
        readProperty(name).fromString(newValue);
        Document query = BUILDER.getName(name);
        Document update = BUILDER.getValue(newValue);
        collection.updateOne(query, new Document(MONGO_SET, update));
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
        for(Document document : collection.find()) {
            Property<?> prop = MAPPER.mapProperty(document);
            mapP.put(prop.getName(), prop);
        }
        return mapP;
    }

    /** {@inheritDoc} */
    public Set<String> listPropertyNames() {
        return readAllProperties().keySet();
    }

}
