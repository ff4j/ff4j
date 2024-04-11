package org.ff4j.mongo.store;

/*-
 * #%L
 * ff4j-store-mongodb
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

import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;
import org.bson.Document;
import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.mongo.MongoDbConstants;
import org.ff4j.mongo.mapper.MongoPropertyMapper;
import org.ff4j.mongo.mapper.PropertyDocumentBuilder;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.utils.Util;

import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import static org.ff4j.mongo.MongoDbConstants.MONGO_SET;

/**
 * PropertyStore based on MongoDB database.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class PropertyStoreMongo extends AbstractPropertyStore {

    /** Property mapper. */
    private MongoPropertyMapper PMAPPER = new MongoPropertyMapper();

    /** Build fields. */
    private static final PropertyDocumentBuilder BUILDER = new PropertyDocumentBuilder();

    /** MongoDB collection. */
    private MongoCollection<Document> propertiesCollection;

    /** Feature collection Name. */
    private String collectionName = MongoDbConstants.DEFAULT_PROPERTY_COLLECTION;

    /** Database name. */
    private String dbName = MongoDbConstants.DEFAULT_DBNAME;

    /** Current mongo client. */
    private MongoClient mongoClient;

    /**
     * Empty constructor.
     */
    public PropertyStoreMongo() {
    }

    /**
     * Parameterized constructor with client and database name.
     *
     * @param client
     *            a mongo client
     * @param dbName
     *            the database name
     */
    public PropertyStoreMongo(MongoClient client, String dbName) {
        this.dbName      = dbName;
        this.mongoClient = client;
        this.propertiesCollection = getPropertiesCollection();
    }

    /**
     * Parameterized constructor a client.
     *
     * @param client
     *            a mongo client
     */
    public PropertyStoreMongo(MongoClient client) {
        this(client, MongoDbConstants.DEFAULT_DBNAME);
    }

    /**
     * Parameterized constructor with client, database name and collection name.
     *
     * @param client
     *            a mongo client
     * @param dbName
     *            the database name
     * @param collectionName
     *            the collection name
     */
    public PropertyStoreMongo(MongoClient client, String dbName, String collectionName) {
        this.mongoClient        = client;
        this.collectionName     = collectionName;
        this.dbName             = dbName;
        this.propertiesCollection = getPropertiesCollection();
    }

    /**
     * Parameterized constructor with database.
     *
     * @param db
     *            the mongo database
     */
    public PropertyStoreMongo(MongoDatabase db) {
        this(db, MongoDbConstants.DEFAULT_PROPERTY_COLLECTION);
    }

    /**
     * Parameterized constructor with database and collection.
     *
     * @param db
     *            the mongo database
     * @param collectionName
     *            the collection name
     */
    public PropertyStoreMongo(MongoDatabase db, String collectionName) {
        this.propertiesCollection = db.getCollection(collectionName);
        this.dbName = db.getName();
    }

    /**
     * Parameterized constructor with collection.
     *
     * @param collection
     *            the collection to set
     */
    public PropertyStoreMongo(MongoCollection<Document> collection) {
        this.propertiesCollection = collection;
    }

    /**
     * Parameterized constructor with collection and XML config file.
     *
     * @param collection
     *            the collection to set
     * @param xmlConfFile
     *            an XML config file
     */
    public PropertyStoreMongo(MongoCollection<Document> collection, String xmlConfFile) {
        this(collection);
        importPropertiesFromXmlFile(xmlConfFile);
    }

    /** {@inheritDoc} */
    public boolean existProperty(String name) {
        Util.assertHasLength(name);
        return 1 == getPropertiesCollection().countDocuments(BUILDER.getName(name));
    }

    /** {@inheritDoc} */
    public <T> void createProperty(Property<T> prop) {
        if (prop == null) {
            throw new IllegalArgumentException("Property cannot be null nor empty");
        }
        if (existProperty(prop.getName())) {
            throw new PropertyAlreadyExistException(prop.getName());
        }
        getPropertiesCollection().insertOne(PMAPPER.toStore(prop));
    }

    /** {@inheritDoc} */
    public Property<?> readProperty(String name) {
        assertPropertyExist(name);
        Document object = getPropertiesCollection().find(BUILDER.getName(name)).first();
        return null != object ? PMAPPER.fromStore(object) : null;
    }

    /** {@inheritDoc} */
    public void deleteProperty(String name) {
        assertPropertyExist(name);
        getPropertiesCollection().deleteOne(BUILDER.getName(name));
    }

    /** {@inheritDoc} */
    public void clear() {
        getPropertiesCollection().deleteMany(new Document());
    }

    /** {@inheritDoc} */
    public void updateProperty(String name, String newValue) {
        assertPropertyExist(name);
        readProperty(name).fromString(newValue);
        Document query = BUILDER.getName(name);
        Document update = BUILDER.getValue(newValue);
        getPropertiesCollection().updateOne(query, new Document(MONGO_SET, update));
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
        LinkedHashMap<String, Property<?>> mapP = new LinkedHashMap<>();
        for(Document document : getPropertiesCollection().find()) {
            Property<?> prop = PMAPPER.fromStore(document);
            mapP.put(prop.getName(), prop);
        }
        return mapP;
    }

    /** {@inheritDoc} */
    public Set<String> listPropertyNames() {
        return readAllProperties().keySet();
    }

    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        if (!mongoClient.getDatabase(dbName)
                .listCollectionNames()
                .into(new HashSet<>())
                .contains(collectionName)) {
            mongoClient.getDatabase(dbName).getCollection(collectionName);
        }
        propertiesCollection = mongoClient.getDatabase(dbName).getCollection(collectionName);
    }

    /**
     * Getter accessor for attribute 'featuresCollection'.
     *
     * @return
     *       current value of 'featuresCollection'
     */
    public MongoCollection<Document> getPropertiesCollection() {
        if (propertiesCollection == null) {
            if (mongoClient != null) {
                createSchema();
            } else {
                throw new IllegalStateException("Cannot initialize Properties collection : no mongo client defined");
            }
        }
        return propertiesCollection;
    }

}
