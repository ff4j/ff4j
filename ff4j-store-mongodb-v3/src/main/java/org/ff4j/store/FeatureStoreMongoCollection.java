package org.ff4j.store;

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
import org.ff4j.store.mongodb.FeatureDocumentBuilder;
import org.ff4j.store.mongodb.FeatureDocumentMapper;
import org.ff4j.utils.Util;

import static org.ff4j.store.mongodb.FeatureStoreMongoConstants.*;

/**
 * Implementation of {@link FeatureStore} to work with MongoDB.
 * 
 * @author William Delanoue (@twillouer) </a>
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FeatureStoreMongoCollection extends AbstractFeatureStore {

    /** Map from DBObject to Feature. */
    private static final FeatureDocumentMapper MAPPER = new FeatureDocumentMapper();

    /** Build fields. */
    private static final FeatureDocumentBuilder BUILDER = new FeatureDocumentBuilder();
    public static final String FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY = "Feature identifier cannot be null nor empty";
    public static final String GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY = "Groupname cannot be null nor empty";

    /** MongoDB collection. */
    private final MongoCollection<Document> collection;

    /**
     * Parameterized constructor with collection.
     * 
     * @param collection
     *            the collection to set
     */
    public FeatureStoreMongoCollection(MongoCollection<Document> collection) {
        this.collection = collection;
    }
    
    /**
     * Parameterized constructor with collection.
     * 
     * @param collection
     *            the collection to set
     */
    public FeatureStoreMongoCollection(MongoCollection<Document> collection, String xmlConfFile) {
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
        collection.updateOne(target, new Document(MONGO_SET, enabledd));
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String featId) {
        Util.assertHasLength(featId);
        return 1 == collection.count(BUILDER.getFeatUid(featId));
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        Document object = collection.find(BUILDER.getFeatUid(uid)).first();
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
        collection.insertOne(MAPPER.toDocument(fp));
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
        collection.deleteOne(BUILDER.getFeatUid(uid));
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
        collection.updateOne(BUILDER.getFeatUid(uid), new Document("$addToSet", BUILDER.getRoles(roleName)));
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
        collection.updateOne(BUILDER.getFeatUid(uid), new Document("$pull", BUILDER.getRoles(roleName)));
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<String, Feature>();
        for(Document document : collection.find()) {
            Feature feature = MAPPER.mapFeature(document);
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
        collection.updateOne(BUILDER.getFeatUid(fp.getUid()), new Document(MONGO_SET, MAPPER.toDocument(fp)));
    }

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        return collection.count(BUILDER.getGroupName(groupName)) > 0;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> readAllGroups() {
        Set<String> setOfGroups = new HashSet<String>();
        for (Document document : collection.find()) {
            setOfGroups.add(document.getString(GROUPNAME));
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
        for (Document document : collection.find(BUILDER.getGroupName(groupName))) {
            Feature feature = MAPPER.mapFeature(document);
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
        for (Document document : collection.find(BUILDER.getGroupName(groupName))) {
            Object enabled = BUILDER.getEnable(true);
            collection.updateOne(document, new Document(MONGO_SET, enabled));
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
        for (Document document: collection.find(BUILDER.getGroupName(groupName))) {
            Object enabled = BUILDER.getEnable(false);
            collection.updateOne(document, new Document(MONGO_SET, enabled));
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
        collection.updateOne(target, new Document(MONGO_SET, nGroupName));
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
        collection.updateOne(target, new Document(MONGO_SET, nGroupName));
    }
    
    /** {@inheritDoc} */
    @Override
    public void clear() {
        collection.deleteMany(new Document());
    }

}
