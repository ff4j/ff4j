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

import java.util.LinkedHashMap;
import java.util.Map;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.store.mongodb.FeatureDBObjectBuilder;
import org.ff4j.store.mongodb.FeatureDBObjectMapper;
import org.ff4j.store.mongodb.FeatureStoreMongoConstants;

import com.mongodb.BasicDBObject;
import com.mongodb.BasicDBObjectBuilder;
import com.mongodb.DBCollection;
import com.mongodb.DBObject;

/**
 * Implementation of {@link FeatureStore} to work with MongoDB.
 * 
 * @author William Delanoue (@twillouer) </a>
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreMongoDB implements FeatureStore, FeatureStoreMongoConstants {

    /** Map from DBObject to Feature. */
    private static final FeatureDBObjectMapper MAPPER = new FeatureDBObjectMapper();

    /** Build fields. */
    private static final FeatureDBObjectBuilder BUILDER = new FeatureDBObjectBuilder();

    /** MongoDB collection. */
    private final DBCollection collection;

    /**
     * Parameterized constructor with collection.
     * 
     * @param collection
     *            the collection to set
     */
    public FeatureStoreMongoDB(DBCollection collection) {
        this.collection = collection;
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
     * @param featId
     *            feature id
     * @param enable
     *            enabler
     */
    private void updateStatus(String featId, boolean enable) {
        if (!exist(featId)) {
            throw new FeatureNotFoundException(featId);
        }
        DBObject target = BUILDER.getFeatUid(featId);
        Object enabledd = BUILDER.getEnable(enable);
        collection.update(target, BasicDBObjectBuilder.start(MONGO_SET, enabledd).get());
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String featId) {
        return 1 == collection.count(BUILDER.getFeatUid(featId));
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String featId) {
        DBObject object = collection.findOne(BUILDER.getFeatUid(featId));
        if (object==null) {
            throw new FeatureNotFoundException(featId);
        }
        return MAPPER.mapFeature(object);
    }

    /** {@inheritDoc} */
    @Override
    public void create(Feature fp) {
        if (exist(fp.getUid())) {
            throw new FeatureAlreadyExistException(fp.getUid());
        }
        collection.save(MAPPER.toDBObject(fp));
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String fpId) {
        if (!exist(fpId)) {
            throw new FeatureNotFoundException(fpId);
        }
        collection.remove(BUILDER.getFeatUid(fpId));
    }

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String fpId, String roleName) {
        if (!exist(fpId)) {
            throw new FeatureNotFoundException(fpId);
        }
        collection.update(BUILDER.getFeatUid(fpId), new BasicDBObject("$addToSet", BUILDER.getRoles(roleName)));
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String fpId, String roleName) {
        if (!exist(fpId)) {
            throw new FeatureNotFoundException(fpId);
        }
        collection.update(BUILDER.getFeatUid(fpId), new BasicDBObject("$pull", BUILDER.getRoles(roleName)));
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<String, Feature>();
        for(DBObject dbObject : collection.find()) {
            Feature feature = MAPPER.mapFeature(dbObject);
            mapFP.put(feature.getUid(), feature);
        }
        return mapFP;
    }

    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        Feature fpExist = read(fp.getUid());
        collection.save(MAPPER.toDBObject(fp));

        // enable/disable
        if (fp.isEnable() != fpExist.isEnable()) {
            if (fp.isEnable()) {
                enable(fp.getUid());
            } else {
                disable(fp.getUid());
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        return collection.count(BUILDER.getGroupName(groupName)) > 0;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<String, Feature>();
        for (DBObject dbObject : collection.find(BUILDER.getGroupName(groupName))) {
            Feature feature = MAPPER.mapFeature(dbObject);
            mapFP.put(feature.getUid(), feature);
        }
        return mapFP;
    }

    /** {@inheritDoc} */
    @Override
    public void enableGroup(String groupName) {
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        for (DBObject dbObject : collection.find(BUILDER.getGroupName(groupName))) {
            Object enabledd = BUILDER.getEnable(true);
            collection.update(dbObject, BasicDBObjectBuilder.start(MONGO_SET, enabledd).get());
        }
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        for (DBObject dbObject : collection.find(BUILDER.getGroupName(groupName))) {
            Object enabledd = BUILDER.getEnable(false);
            collection.update(dbObject, BasicDBObjectBuilder.start(MONGO_SET, enabledd).get());
        }
    }

    /** {@inheritDoc} */
    @Override
    public void addToGroup(String featureId, String groupName) {
        if (!exist(featureId)) {
            throw new FeatureNotFoundException(featureId);
        }
        DBObject target = BUILDER.getFeatUid(featureId);
        DBObject nGroupName = BUILDER.getGroupName(groupName);
        collection.update(target, BasicDBObjectBuilder.start(MONGO_SET, nGroupName).get());
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String featureId, String groupName) {
        if (!exist(featureId)) {
            throw new FeatureNotFoundException(featureId);
        }
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        DBObject target = BUILDER.getFeatUid(featureId);
        DBObject nGroupName = BUILDER.getGroupName("");
        collection.update(target, BasicDBObjectBuilder.start(MONGO_SET, nGroupName).get());
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return "FeatureStoreMongoDB [collection=" + collection.getFullName() + "]";
    }

}
