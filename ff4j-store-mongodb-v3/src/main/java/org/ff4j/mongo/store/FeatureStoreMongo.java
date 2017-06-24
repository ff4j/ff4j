package org.ff4j.mongo.store;

import static org.ff4j.mongo.MongoDbConstants.MONGO_SET;

import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.bson.Document;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.mongo.MongoDbConstants;
import org.ff4j.mongo.mapper.FeatureDocumentBuilder;
import org.ff4j.mongo.mapper.MongoFeatureMapper;
import org.ff4j.store.AbstractFeatureStore;
import org.ff4j.utils.Util;

import com.mongodb.MongoClient;

/*
 * #%L ff4j-store-jdbc %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;

/**
 * Implementation of {@link FeatureStore} to work with MongoDB.
 * 
 * @author William Delanoue (@twillouer) </a>
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FeatureStoreMongo extends AbstractFeatureStore {

    /** Map from DBObject to Feature. */
    private static final MongoFeatureMapper FMAPPER = new MongoFeatureMapper();

    /** Build fields. */
    private static final FeatureDocumentBuilder BUILDER = new FeatureDocumentBuilder();
    
    /** error message. */
    public static final String FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY = "Feature identifier cannot be null nor empty";
    
    /** error message. */
    public static final String GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY = "Groupname cannot be null nor empty";

    /** MongoDB collection. */
    private MongoCollection<Document> featuresCollection;
    
    /** Feature collection Name. */
    private String collectionName = MongoDbConstants.DEFAULT_FEATURE_COLLECTION;
    
    /** Database name. */
    private String dbName = MongoDbConstants.DEFAULT_DBNAME;
    
    /** Current mongo client. */
    private MongoClient mongoClient;

    /**
     * Parameterized constructor with collection.
     * 
     * @param collection
     *            the collection to set
     */
    public FeatureStoreMongo() {
    }
    
    /**
     * Parameterized constructor with collection.
     * 
     * @param collection
     *            the collection to set
     */
    public FeatureStoreMongo(MongoClient client) {
        this(client, MongoDbConstants.DEFAULT_DBNAME);
    }
    
    /**
     * Parameterized constructor with collection.
     * 
     * @param collection
     *            the collection to set
     */
    public FeatureStoreMongo(MongoClient client, String dbName) {
        this.dbName      = dbName;
        this.mongoClient = client;
        this.featuresCollection = getFeaturesCollection();
    }
    
    /**
     * Parameterized constructor with collection.
     * 
     * @param collection
     *            the collection to set
     */
    public FeatureStoreMongo(MongoClient client, String dbName, String collectionName) {
        this.mongoClient        = client;
        this.collectionName     = collectionName;
        this.dbName             = dbName;
        this.featuresCollection = getFeaturesCollection();
    }
    
    /**
     * Parameterized constructor with collection.
     * 
     * @param collection
     *            the collection to set
     */
    public FeatureStoreMongo(MongoDatabase db, MongoClient client) {
        this(db, MongoDbConstants.DEFAULT_FEATURE_COLLECTION);
        this.mongoClient = client;
    }
    
    /**
     * Parameterized constructor with collection.
     * 
     * @param collection
     *            the collection to set
     */
    public FeatureStoreMongo(MongoDatabase db, String collectionName) {
        this.dbName = db.getName();
        this.featuresCollection = db.getCollection(collectionName);
    }
    
    /**
     * Parameterized constructor with collection.
     * 
     * @param collection
     *            the collection to set
     */
    public FeatureStoreMongo(MongoCollection<Document> collection) {
        this.featuresCollection = collection;
    }
    
    /**
     * Parameterized constructor with collection.
     * 
     * @param collection
     *            the collection to set
     */
    public FeatureStoreMongo(MongoCollection<Document> collection, String xmlConfFile) {
        this(collection);
        importFeaturesFromXmlFile(xmlConfFile);
    }

    /** {@inheritDoc} */
    @Override
    public void enable(String featId) {
        this.updateStatus(featId, true);
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String featId) {
        this.updateStatus(featId, false);
    }

    /**
     * Update status of feature.
     * 
     * @param uid
     *            feature id
     * @param enable
     *            enabler
     */
    private void updateStatus(String uid, boolean enable) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        Document target = BUILDER.getFeatUid(uid);
        Object enabledd = BUILDER.getEnable(enable);
        getFeaturesCollection().updateOne(target, new Document(MONGO_SET, enabledd));
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String featId) {
        Util.assertHasLength(featId);
        return 1 == getFeaturesCollection().count(BUILDER.getFeatUid(featId));
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        Document object = getFeaturesCollection().find(BUILDER.getFeatUid(uid)).first();
        if (object==null) {
            throw new FeatureNotFoundException(uid);
        }
        return FMAPPER.fromStore(object);
    }

    /** {@inheritDoc} */
    @Override
    public void create(Feature fp) {
        if (fp == null) {
            throw new IllegalArgumentException("Feature cannot be null nor empty");
        }
        if (exist(fp.getUid())) {
            throw new FeatureAlreadyExistException(fp.getUid());
        }
        getFeaturesCollection().insertOne(FMAPPER.toStore(fp));
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        getFeaturesCollection().deleteOne(BUILDER.getFeatUid(uid));
    }

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String uid, String roleName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (roleName == null || roleName.isEmpty()) {
            throw new IllegalArgumentException("roleName cannot be null nor empty");
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        getFeaturesCollection().updateOne(BUILDER.getFeatUid(uid), new Document("$addToSet", BUILDER.getRoles(roleName)));
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String uid, String roleName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (roleName == null || roleName.isEmpty()) {
            throw new IllegalArgumentException("roleName cannot be null nor empty");
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        getFeaturesCollection().updateOne(BUILDER.getFeatUid(uid), new Document("$pull", BUILDER.getRoles(roleName)));
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<String, Feature>();
        for(Document document : getFeaturesCollection().find()) {
            Feature feature = FMAPPER.fromStore(document);
            mapFP.put(feature.getUid(), feature);
        }
        return mapFP;
    }

    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        if (fp == null) {
            throw new IllegalArgumentException("Feature cannot be null nor empty");
        }
        read(fp.getUid());
        getFeaturesCollection().updateOne(BUILDER.getFeatUid(fp.getUid()), new Document(MONGO_SET, FMAPPER.toStore(fp)));
    }

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        return getFeaturesCollection().count(BUILDER.getGroupName(groupName)) > 0;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> readAllGroups() {
        Set<String> setOfGroups = new HashSet<String>();
        for (Document document : getFeaturesCollection().find()) {
            setOfGroups.add(document.getString(MongoDbConstants.FEATURE_GROUPNAME));
        }
        setOfGroups.remove(null);
        setOfGroups.remove("");
        return setOfGroups;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<String, Feature>();
        for (Document document : getFeaturesCollection().find(BUILDER.getGroupName(groupName))) {
            Feature feature = FMAPPER.fromStore(document);
            mapFP.put(feature.getUid(), feature);
        }
        return mapFP;
    }

    /** {@inheritDoc} */
    @Override
    public void enableGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        for (Document document : getFeaturesCollection().find(BUILDER.getGroupName(groupName))) {
            Object enabled = BUILDER.getEnable(true);
            getFeaturesCollection().updateOne(document, new Document(MONGO_SET, enabled));
        }
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        for (Document document: getFeaturesCollection().find(BUILDER.getGroupName(groupName))) {
            Object enabled = BUILDER.getEnable(false);
            getFeaturesCollection().updateOne(document, new Document(MONGO_SET, enabled));
        }
    }

    /** {@inheritDoc} */
    @Override
    public void addToGroup(String uid, String groupName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        Document target = BUILDER.getFeatUid(uid);
        Document nGroupName = BUILDER.getGroupName(groupName);
        getFeaturesCollection().updateOne(target, new Document(MONGO_SET, nGroupName));
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String uid, String groupName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        Document target = BUILDER.getFeatUid(uid);
        Document nGroupName = BUILDER.getGroupName("");
        getFeaturesCollection().updateOne(target, new Document(MONGO_SET, nGroupName));
    }
    
    /** {@inheritDoc} */
    @Override
    public void clear() {
        getFeaturesCollection().deleteMany(new Document());
    }

    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        if (!mongoClient.getDatabase(dbName)
                .listCollectionNames()
                .into(new HashSet<String>())
                .contains(collectionName)) {
            mongoClient.getDatabase(dbName).createCollection(collectionName);
        }
        featuresCollection = mongoClient.getDatabase(dbName).getCollection(collectionName);
    }
    
    /**
     * Getter accessor for attribute 'featuresCollection'.
     *
     * @return
     *       current value of 'featuresCollection'
     */
    public MongoCollection<Document> getFeaturesCollection() {
        if (featuresCollection == null) {
            if (mongoClient != null) {
                createSchema();
            } else {
                throw new IllegalStateException("Cannot initialize Features collection : no mongo client defined");
            }
        }
        return featuresCollection;
    }

}
