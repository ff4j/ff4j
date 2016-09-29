package org.ff4j.store;

import static org.ff4j.store.mongodb.FeatureStoreMongoConstants.DEFAULT_COLLECTIONAME_FEATURES;
import static org.ff4j.store.mongodb.FeatureStoreMongoConstants.DEFAULT_DBNAME;
import static org.ff4j.store.mongodb.FeatureStoreMongoConstants.GROUPNAME;
import static org.ff4j.store.mongodb.FeatureStoreMongoConstants.MONGO_SET;

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

import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.store.mongodb.FeatureDBObjectBuilder;
import org.ff4j.store.mongodb.FeatureDBObjectMapper;
import org.ff4j.utils.Util;

import com.mongodb.BasicDBObject;
import com.mongodb.BasicDBObjectBuilder;
import com.mongodb.DBCollection;
import com.mongodb.DBObject;
import com.mongodb.MongoClient;

/**
 * Implementation of {@link FeatureStore} to work with MongoDB.
 * 
 * @author William Delanoue (@twillouer)
 * @author Cedrick LUNVEN (@clunven)
 */
public class FeatureStoreMongoDB extends AbstractFeatureStore {

    /** Build fields. */
    public static final String FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY = "Feature identifier cannot be null nor empty";
    
    /** Build fields. */
    public static final String GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY = "Groupname cannot be null nor empty";

    /** Map from DBObject to Feature. */
    private static final FeatureDBObjectMapper MAPPER = new FeatureDBObjectMapper();

    /** Build fields. */
    private static final FeatureDBObjectBuilder BUILDER = new FeatureDBObjectBuilder();
    
    /** Feature collection Name. */
    private String collectionName = DEFAULT_COLLECTIONAME_FEATURES;
    
    /** Database name. */
    private String dbName = DEFAULT_DBNAME;
    
    /** Mongo Client. */
    private MongoClient mongoClient;
   
    /** MongoDB collection. */
    private DBCollection featuresCollection;
    
    /**
     * Parameterized constructor with collection.
     * 
     * @param collection
     *            the collection to set
     */
    public FeatureStoreMongoDB() {
    }
    
    /**
     * Parameterized constructor with collection.
     * 
     * @param collection
     *            the collection to set
     */
    public FeatureStoreMongoDB(MongoClient client) {
        this.mongoClient = client;
        this.featuresCollection = getFeaturesCollection();
    }
            
    /**
     * Parameterized constructor with collection.
     * 
     * @param collection
     *            the collection to set
     */
    public FeatureStoreMongoDB(MongoClient client, String dbName, String collectionName) {
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
    public FeatureStoreMongoDB(DBCollection collection) {
        this.featuresCollection = collection;
        this.collectionName     = featuresCollection.getName();
        this.dbName             = featuresCollection.getDB().getName();
    }
    
    /**
     * Parameterized constructor with collection.
     * 
     * @param collection
     *            the collection to set
     */
    public FeatureStoreMongoDB(DBCollection collection, String xmlConfFile) {
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
        Util.assertParamHasLength(uid, "uid (feature identifier");
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        DBObject target = BUILDER.getFeatUid(uid);
        Object enabledd = BUILDER.getEnable(enable);
        getFeaturesCollection().update(target, BasicDBObjectBuilder.start(MONGO_SET, enabledd).get());
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
        DBObject object = getFeaturesCollection().findOne(BUILDER.getFeatUid(uid));
        if (object==null) {
            throw new FeatureNotFoundException(uid);
        }
        return MAPPER.mapFeature(object);
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
        getFeaturesCollection().save(MAPPER.toDBObject(fp));
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
        getFeaturesCollection().remove(BUILDER.getFeatUid(uid));
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
        getFeaturesCollection().update(BUILDER.getFeatUid(uid), new BasicDBObject("$addToSet", BUILDER.getRoles(roleName)));
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
        getFeaturesCollection().update(BUILDER.getFeatUid(uid), new BasicDBObject("$pull", BUILDER.getRoles(roleName)));
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<String, Feature>();
        for(DBObject dbObject : getFeaturesCollection().find()) {
            Feature feature = MAPPER.mapFeature(dbObject);
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
        getFeaturesCollection().save(MAPPER.toDBObject(fp));
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
        for (DBObject dbObject : getFeaturesCollection().find()) {
            setOfGroups.add((String) dbObject.get(GROUPNAME));
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
        for (DBObject dbObject : getFeaturesCollection().find(BUILDER.getGroupName(groupName))) {
            Feature feature = MAPPER.mapFeature(dbObject);
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
        for (DBObject dbObject : getFeaturesCollection().find(BUILDER.getGroupName(groupName))) {
            Object enabledd = BUILDER.getEnable(true);
            getFeaturesCollection().update(dbObject, BasicDBObjectBuilder.start(MONGO_SET, enabledd).get());
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
        for (DBObject dbObject : getFeaturesCollection().find(BUILDER.getGroupName(groupName))) {
            Object enabledd = BUILDER.getEnable(false);
            getFeaturesCollection().update(dbObject, BasicDBObjectBuilder.start(MONGO_SET, enabledd).get());
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
        DBObject target = BUILDER.getFeatUid(uid);
        DBObject nGroupName = BUILDER.getGroupName(groupName);
        getFeaturesCollection().update(target, BasicDBObjectBuilder.start(MONGO_SET, nGroupName).get());
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
        DBObject target = BUILDER.getFeatUid(uid);
        DBObject nGroupName = BUILDER.getGroupName("");
        getFeaturesCollection().update(target, BasicDBObjectBuilder.start(MONGO_SET, nGroupName).get());
    }
    
    /** {@inheritDoc} */
    @Override
    public void clear() {
        getFeaturesCollection().remove(BasicDBObjectBuilder.start().get());
    }

    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        if (!mongoClient.getDB(dbName).collectionExists(collectionName)) {
            BasicDBObject options =  new BasicDBObject();
            options.put("size", 10000);
            featuresCollection = mongoClient.getDB(dbName).createCollection(collectionName, options);
        }
        featuresCollection = mongoClient.getDB(dbName).getCollection(collectionName);
    }

    /**
     * Getter accessor for attribute 'featuresCollection'.
     *
     * @return
     *       current value of 'featuresCollection'
     */
    public DBCollection getFeaturesCollection() {
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
