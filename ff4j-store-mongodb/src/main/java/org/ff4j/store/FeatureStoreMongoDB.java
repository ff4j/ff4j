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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.management.RuntimeErrorException;
import javax.sql.DataSource;

import org.bson.BSONObject;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.store.mongodb.FeatureDBObjectBuilder;
import org.ff4j.store.mongodb.FeatureDBObjectMapper;
import org.ff4j.store.mongodb.FeatureStoreMongoConstants;
import org.ff4j.utils.ParameterUtils;

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
public class FeatureStoreMongoDB implements FeatureStore,
		FeatureStoreMongoConstants {

	private static final String GROUP_NAME_AND_REGION_BOTH_CAN_NOT_BE_NULL = "Group name and region both can not be null";

	private static final String FEATURE_CANNOT_BE_NULL_NOR_EMPTY = "Feature cannot be null nor empty";

	private static final String GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY = "Groupname cannot be null nor empty";

	private static final String ROLE_NAME_CANNOT_BE_NULL_NOR_EMPTY = "roleName cannot be null nor empty";

	private static final String FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY = "Feature identifier cannot be null nor empty";

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
	private void updateStatus(String uid, boolean enable) {
		if (uid == null || uid.isEmpty()) {
			throw new IllegalArgumentException(
					FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
		}
		if (!exist(uid)) {
			throw new FeatureNotFoundException(uid);
		}
		DBObject target = BUILDER.getFeatUid(uid);
		Object enabledd = BUILDER.getEnable(enable);
		collection.update(target,
				BasicDBObjectBuilder.start(MONGO_SET, enabledd).get());
	}

	/** {@inheritDoc} */
	@Override
	public boolean exist(String featId) {
		return 1 <= collection.count(BUILDER.getFeatUid(featId));
	}

	/** {@inheritDoc} */
	@Override
	public Feature read(String uid) {
		if (uid == null || uid.isEmpty()) {
			throw new IllegalArgumentException(
					FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
		}
		DBObject object = collection.findOne(BUILDER.getFeatUid(uid));
		if (object == null) {
			throw new FeatureNotFoundException(uid);
		}
		return MAPPER.mapFeature(object);
	}

	/** {@inheritDoc} */
	@Override
	public void create(Feature fp) {
		if (fp == null) {
			throw new IllegalArgumentException(FEATURE_CANNOT_BE_NULL_NOR_EMPTY);
		}
		if (exist(fp.getUid(), fp.getGroup(), fp.getRegionIdentifier())) {
			throw new FeatureAlreadyExistException(fp.getUid());
		}
		collection.insert(MAPPER.toDBObject(fp));
	}

	/** {@inheritDoc} */
	@Override
	public void delete(String uid) {
		if (uid == null || uid.isEmpty()) {
			throw new IllegalArgumentException(
					FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
		}
		if (!exist(uid)) {
			throw new FeatureNotFoundException(uid);
		}
		collection.remove(BUILDER.getFeatUid(uid));
	}

	/** {@inheritDoc} */
	@Override
	public void grantRoleOnFeature(String uid, String roleName) {
		if (uid == null || uid.isEmpty()) {
			throw new IllegalArgumentException(
					FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
		}
		if (roleName == null || roleName.isEmpty()) {
			throw new IllegalArgumentException(
					ROLE_NAME_CANNOT_BE_NULL_NOR_EMPTY);
		}
		if (!exist(uid)) {
			throw new FeatureNotFoundException(uid);
		}
		collection.update(BUILDER.getFeatUid(uid), new BasicDBObject(
				"$addToSet", BUILDER.getRoles(roleName)));
	}

	/** {@inheritDoc} */
	@Override
	public void removeRoleFromFeature(String uid, String roleName) {
		if (uid == null || uid.isEmpty()) {
			throw new IllegalArgumentException(
					FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
		}
		if (roleName == null || roleName.isEmpty()) {
			throw new IllegalArgumentException(
					ROLE_NAME_CANNOT_BE_NULL_NOR_EMPTY);
		}
		if (!exist(uid)) {
			throw new FeatureNotFoundException(uid);
		}
		collection.update(BUILDER.getFeatUid(uid), new BasicDBObject("$pull",
				BUILDER.getRoles(roleName)));
	}

	/** {@inheritDoc} */
	@Override
	public Map<String, Feature> readAll() {
		LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<String, Feature>();
		for (DBObject dbObject : collection.find()) {
			Feature feature = MAPPER.mapFeature(dbObject);
			mapFP.put(feature.getUid(), feature);
		}
		return mapFP;
	}

	/** {@inheritDoc} */
	@Override
	public void update(Feature fp) {
		if (fp == null) {
			throw new IllegalArgumentException(FEATURE_CANNOT_BE_NULL_NOR_EMPTY);
		}
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
		if (groupName == null || groupName.isEmpty()) {
			throw new IllegalArgumentException(
					GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
		}
		return collection.count(BUILDER.getGroupName(groupName)) > 0;
	}

	/** {@inheritDoc} */
	@Override
	public Set<String> readAllGroups() {
		Set<String> setOfGroups = new HashSet<String>();
		for (DBObject dbObject : collection.find()) {
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
			throw new IllegalArgumentException(
					GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
		}
		if (!existGroup(groupName)) {
			throw new GroupNotFoundException(groupName);
		}
		LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<String, Feature>();
		for (DBObject dbObject : collection.find(BUILDER
				.getGroupName(groupName))) {
			Feature feature = MAPPER.mapFeature(dbObject);
			mapFP.put(feature.getUid(), feature);
		}
		return mapFP;
	}

	/** {@inheritDoc} */
	@Override
	public void enableGroup(String groupName) {
		if (groupName == null || groupName.isEmpty()) {
			throw new IllegalArgumentException(
					GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
		}
		if (!existGroup(groupName)) {
			throw new GroupNotFoundException(groupName);
		}
		for (DBObject dbObject : collection.find(BUILDER
				.getGroupName(groupName))) {
			Object enabledd = BUILDER.getEnable(true);
			collection.update(dbObject,
					BasicDBObjectBuilder.start(MONGO_SET, enabledd).get());
		}
	}

	/** {@inheritDoc} */
	@Override
	public void disableGroup(String groupName) {
		if (groupName == null || groupName.isEmpty()) {
			throw new IllegalArgumentException(
					GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
		}
		if (!existGroup(groupName)) {
			throw new GroupNotFoundException(groupName);
		}
		for (DBObject dbObject : collection.find(BUILDER
				.getGroupName(groupName))) {
			Object enabledd = BUILDER.getEnable(false);
			collection.update(dbObject,
					BasicDBObjectBuilder.start(MONGO_SET, enabledd).get());
		}
	}

	/** {@inheritDoc} */
	@Override
	public void addToGroup(String uid, String groupName) {
		if (uid == null || uid.isEmpty()) {
			throw new IllegalArgumentException(
					FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
		}
		if (groupName == null || groupName.isEmpty()) {
			throw new IllegalArgumentException(
					GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
		}
		if (!exist(uid)) {
			throw new FeatureNotFoundException(uid);
		}
		DBObject target = BUILDER.getFeatUid(uid);
		DBObject nGroupName = BUILDER.getGroupName(groupName);
		collection.update(target,
				BasicDBObjectBuilder.start(MONGO_SET, nGroupName).get());
	}

	/** {@inheritDoc} */
	@Override
	public void removeFromGroup(String uid, String groupName) {
		if (uid == null || uid.isEmpty()) {
			throw new IllegalArgumentException(
					FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
		}
		if (groupName == null || groupName.isEmpty()) {
			throw new IllegalArgumentException(
					GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
		}
		if (!exist(uid)) {
			throw new FeatureNotFoundException(uid);
		}
		if (!existGroup(groupName)) {
			throw new GroupNotFoundException(groupName);
		}
		DBObject target = BUILDER.getFeatUid(uid);
		DBObject nGroupName = BUILDER.getGroupName("");
		collection.update(target,
				BasicDBObjectBuilder.start(MONGO_SET, nGroupName).get());
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder("{");
		sb.append("\"type\":\"" + this.getClass().getCanonicalName() + "\"");
		sb.append("\"mongodb\":\"" + this.collection.getName() + "\"");
		Set<String> myFeatures = readAll().keySet();
		sb.append(",\"numberOfFeatures\":" + myFeatures.size());
		sb.append(",\"features\":[");
		boolean first = true;
		for (String myFeature : myFeatures) {
			if (!first) {
				sb.append(",");
			}
			first = false;
			sb.append("\"" + myFeature + "\"");
		}
		Set<String> myGroups = readAllGroups();
		sb.append("],\"numberOfGroups\":" + myGroups.size());
		sb.append(",\"groups\":[");
		first = true;
		for (String myGroup : myGroups) {
			if (!first) {
				sb.append(",");
			}
			first = false;
			sb.append("\"" + myGroup + "\"");
		}
		sb.append("]");
		sb.append("}");
		return sb.toString();
	}

	// Logic to fetch feature based on group and region

	public boolean exist(String featId, String groupName, String region) {

		return 1 == collection.count(BUILDER.getFeatUid(featId, groupName,
				region));
	}

	/**
	 * Enable features based by group name and region
	 * 
	 * @param featId
	 *            feature name
	 * @param groupName
	 *            group of the feature, could be null
	 * @param region
	 *            , environment of the feature.
	 */
	public void enable(String featId, String groupName, String region) {
		if (!exist(featId, groupName, region)) {
			throw new FeatureNotFoundException(featId);
		}
		this.updateStatus(featId, groupName, region, true);
	}

	/**
	 * Disable features based by group name and region
	 * 
	 * @param featId
	 *            feature name
	 * @param groupName
	 *            group of the feature, could be null
	 * @param region
	 *            , environment of the feature.
	 */
	public void disable(String featId, String groupName, String region) {
		if (!exist(featId, groupName, region)) {
			throw new FeatureNotFoundException(featId);
		}
		this.updateStatus(featId, groupName, region, false);
	}

	/**
	 * Update status of feature.
	 * 
	 * @param featId
	 *            feature id
	 * @param enable
	 *            enabler
	 */
	private void updateStatus(String uid, String groupName, String region,
			boolean enable) {
		if (uid == null || uid.isEmpty()) {
			throw new IllegalArgumentException(
					FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
		}
		if (!exist(uid, groupName, region)) {
			throw new FeatureNotFoundException(uid);
		}
		DBObject target = BUILDER.getFeatUid(uid, groupName, region);
		Object enabledd = BUILDER.getEnable(enable);
		collection.update(target,
				BasicDBObjectBuilder.start(MONGO_SET, enabledd).get());
	}

	public Map<String, Feature> readAll(String groupName, String region) {

		LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<String, Feature>();

		BasicDBObject searchQuery = new BasicDBObject();

		if (groupName != null && region != null) {

			searchQuery.put(GROUPNAME, groupName);
			searchQuery.put(REGION_IDENTIFIER, region);
			setAllDocumentMentsForGivenSearchCriteria(mapFP, searchQuery);

		} else if (groupName == null && region != null) {

			searchQuery.put(REGION_IDENTIFIER, region);
			setAllDocumentMentsForGivenSearchCriteria(mapFP, searchQuery);

		} else {

			new RuntimeException(GROUP_NAME_AND_REGION_BOTH_CAN_NOT_BE_NULL);
		}

		return mapFP;
	}

	private void setAllDocumentMentsForGivenSearchCriteria(
			LinkedHashMap<String, Feature> mapFP, BasicDBObject searchQuery) {

		for (DBObject dbObject : collection.find(searchQuery)) {
			Feature feature = MAPPER.mapFeature(dbObject);
			mapFP.put(feature.getUid(), feature);
		}
	}

	public void delete(String uid, String groupName, String region) {
		if (!exist(uid, groupName, region)) {
			throw new FeatureNotFoundException(uid);
		}

		DBObject target = BUILDER.getFeatUid(uid, groupName, region);
		collection.remove(target);
	}

	public List<String> readAllGroups(String region) {

		DBObject searchObject = new BasicDBObject();
		searchObject.put(REGION_IDENTIFIER, region);
		List<String> groups = new ArrayList<String>();
		
		for (DBObject dbObject : collection.find(searchObject)) {
			
			Feature feature = MAPPER.mapFeature(dbObject);
			groups.add(feature.getGroup());
		}
		
		return groups;

	}
	
	public void updateByGroupRegion(Feature fp) { 
		
		if (fp == null) {
			throw new IllegalArgumentException(
					FEATURE_CANNOT_BE_NULL_NOR_EMPTY);
		}
		if (!exist(fp.getUid(), fp.getGroup(), fp.getRegionIdentifier())) {
			throw new FeatureNotFoundException(fp.getUid());
		}
		DBObject target = BUILDER.getFeatUid(fp.getUid(), fp.getGroup(), fp.getRegionIdentifier());
		DBObject featureObject = MAPPER.toDBObject(fp);
		collection.update(target, featureObject, false, false);	
	}
	
    // -------- Overrided in cache proxy --------------

    /** {@inheritDoc} */
    @Override
    public boolean isCached() {
        return false;
    }

    /** {@inheritDoc} */
    @Override
    public String getCacheProvider() {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public String getCachedTargetStore() {
        return null;
    }


}
