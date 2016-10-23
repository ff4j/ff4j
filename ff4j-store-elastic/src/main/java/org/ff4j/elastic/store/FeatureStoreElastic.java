package org.ff4j.elastic.store;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.search.builder.SearchSourceBuilder;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.elastic.ElasticConnection;
import org.ff4j.elastic.ElasticQueryBuilder;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.store.AbstractFeatureStore;
import org.ff4j.utils.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.searchbox.core.DocumentResult;
import io.searchbox.core.Search;
import io.searchbox.core.SearchResult;
import io.searchbox.core.SearchResult.Hit;
import io.searchbox.core.Update;

/*
 * #%L
 * ff4j-store-elastic
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

/**
 * Implementation of the {@link FeatureStore} to work ElasticSearch storage DB.
 * 
 * @since 1.6
 * 
 * @author C&eacute;drick Lunven (@clunven)
 * @author <a href="mailto:andre.blaszczyk@gmail.com">Andre Blaszczyk</a>
 */
public class FeatureStoreElastic extends AbstractFeatureStore {

	/** Logger for the class. */
	private final Logger logger = LoggerFactory.getLogger(FeatureStoreElastic.class);

	/** Injection of connection to elastic. */
	private ElasticConnection connection;

	/** Connection to store Elastic. */
	private ElasticQueryBuilder builder;

	/**
	 * Default constructor.
	 */
	public FeatureStoreElastic() {
	}

	/**
	 * Initialization through {@link ElasticConnection}.
	 *
	 * @param connection
	 *            current client to Elasticsearch database
	 */
	public FeatureStoreElastic(ElasticConnection connection) {
		this.connection = connection;
	}

	/**
	 * Initialization with Connection and initialisation file.
	 *
	 * @param connection
	 * @param xmlFile
	 */
	public FeatureStoreElastic(ElasticConnection connection, String xmlFile) {
		this(connection);
		importFeaturesFromXmlFile(xmlFile);
		getConnection().execute(getBuilder().queryFlushIndex());
	}

	/** {@inheritDoc} */
	@Override
	public void enable(String uid) {
		assertFeatureExist(uid);
		getConnection().execute(getBuilder().queryEnable(uid));
	}

	/** {@inheritDoc} */
	@Override
	public void disable(String uid) {
		assertFeatureExist(uid);
		getConnection().execute(getBuilder().queryDisable(uid));
	}

	/** {@inheritDoc} */
	@Override
	public boolean exist(String uid) {
		Util.assertHasLength(uid);
		SearchResult result = getConnection().search(getBuilder().queryGetFeatureById(uid), true);
		return (result.getTotal() != null) && (result.getTotal() >= 1);
	}

	/** {@inheritDoc} */
	@Override
	public void create(Feature fp) {
		assertFeatureNotNull(fp);
		assertFeatureNotExist(fp.getUid());
		getConnection().execute(getBuilder().queryCreateFeature(fp));
	}

	/** {@inheritDoc} */
	@Override
	public Feature read(String uid) {
		assertFeatureExist(uid);
		// first hit is ensured as feature exist
		return getConnection().search(getBuilder().queryGetFeatureById(uid)).getFirstHit(Feature.class).source;
	}

	/** {@inheritDoc} */
	@Override
	public Map<String, Feature> readAll() {

		SearchResult result = getConnection().search(getBuilder().queryReadAllFeatures(), true);

		Map<String, Feature> mapOfFeatures = new HashMap<String, Feature>();
		if (null != result && result.isSucceeded()) {
			for (Hit<Feature, Void> feature : result.getHits(Feature.class)) {
				mapOfFeatures.put(feature.source.getUid(), feature.source);
			}
		}
		return mapOfFeatures;
	}

	/** {@inheritDoc} */
	@Override
	public void delete(String uid) {
		assertFeatureExist(uid);
		getConnection().execute(getBuilder().queryDeleteFeature(uid));
	}

	/** {@inheritDoc} */
	@Override
	public void update(Feature fp) {
		assertFeatureNotNull(fp);
		assertFeatureExist(fp.getUid());
		getConnection().execute(getBuilder().queryUpdateFeature(fp));
	}

	/** {@inheritDoc} */
	@Override
	public void grantRoleOnFeature(String flipId, String roleName) {
		assertFeatureExist(flipId);
		Util.assertHasLength(roleName);

		Feature feature = read(flipId);
		feature.getPermissions().add(roleName);
		getConnection().execute(getBuilder().queryUpdateFeature(feature));
	}

	/** {@inheritDoc} */
	@Override
	public void removeRoleFromFeature(String flipId, String roleName) {
		assertFeatureExist(flipId);
		Util.assertHasLength(roleName);

		Feature feature = read(flipId);
		feature.getPermissions().remove(roleName);
		getConnection().execute(getBuilder().queryUpdateFeature(feature));
	}

	/** {@inheritDoc} */
	@Override
	public void enableGroup(String groupName) {
		assertGroupExist(groupName);
		for (String _id : getBuilder().getFeatureTechIdByGroup(groupName)) {
			getConnection().execute(getBuilder().queryEnableWithTechId(_id));
		}
	}

	/** {@inheritDoc} */
	@Override
	public void disableGroup(String groupName) {
		assertGroupExist(groupName);
		for (String _id : getBuilder().getFeatureTechIdByGroup(groupName)) {
			getConnection().execute(getBuilder().queryDisableWithTechId(_id));
		}
	}

	/** {@inheritDoc} */
	@Override
	public boolean existGroup(String groupName) {
		Util.assertParamHasLength(groupName, "groupName");
		SearchResult result = getConnection().search(getBuilder().getGroupByGroupName(groupName), true);
		return (result.getTotal() != null) && (result.getTotal() >= 1);
	}

	// TODO.. continuer refactoring

	/** {@inheritDoc} */
	@Override
	public Map<String, Feature> readGroup(String groupName) {

		// Checking parameter
		if (groupName == null || groupName.isEmpty()) {
			throw new IllegalArgumentException("Groupname cannot be null nor empty");
		}
		if (!existGroup(groupName)) {
			throw new GroupNotFoundException(groupName);
		}

		// Jest request
		SearchSourceBuilder searchSourceBuilder = new SearchSourceBuilder();
		searchSourceBuilder.query(QueryBuilders.matchQuery("group", groupName));

		Search search = new Search.Builder(searchSourceBuilder.toString()) //
				.addIndex(connection.getIndexName()) //
				.addType("feature") //
				.build();

		LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<String, Feature>();

		try {
			SearchResult result = getConnection().getJestClient().execute(search);
			if (!result.isSucceeded()) {
				logger.error(result.getErrorMessage());
			}
			List<Hit<Feature, Void>> features = result.getHits(Feature.class);
			for (Hit<Feature, Void> hit : features) {
				mapFP.put(hit.source.getUid(), hit.source);
			}
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
		}

		return mapFP;
	}

	/** {@inheritDoc} */
	@Override
	public void addToGroup(String featureId, String groupName) {

		// Checking parameters
		if (featureId == null || featureId.isEmpty()) {
			throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
		}
		if (groupName == null || groupName.isEmpty()) {
			throw new IllegalArgumentException("Groupname cannot be null nor empty");
		}

		// Getting metadata id
		String _id = getBuilder().getFeatureTechId(featureId);
		if (_id == null) {
			throw new FeatureNotFoundException(featureId);
		}

		// Partial update
		String partialDoc = "{\n" + //
				" \"doc\" : {\n" + //
				"      \"group\" : \"" + groupName + "\"\n" + //
				"  }\n" + //
				"}"; //

		// Jest request
		Update update = new Update.Builder(partialDoc) //
				.index(connection.getIndexName()) //
				.type("feature") //
				.id(_id) //
				.build();

		try {
			DocumentResult result = getConnection().getJestClient().execute(update);
			if (!result.isSucceeded()) {
				logger.error(result.getErrorMessage());
			}
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
		}
	}

	/** {@inheritDoc} */
	@Override
	public void removeFromGroup(String featureId, String groupName) {

		// Checking parameters
		if (featureId == null || featureId.isEmpty()) {
			throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
		}
		if (groupName == null || groupName.isEmpty()) {
			throw new IllegalArgumentException("Groupname cannot be null nor empty");
		}

		// Getting metadata id
		String _id = getBuilder().getFeatureTechId(featureId);
		if (_id == null) {
			throw new FeatureNotFoundException(featureId);
		}

		// Checking if the given group exists
		if (!existGroup(groupName)) {
			throw new GroupNotFoundException(groupName);
		}

		// Partial update
		String partialDoc = "{\n" + //
				" \"doc\" : {\n" + //
				"      \"group\" : \"\"\n" + //
				"  }\n" + //
				"}"; //

		// Jest request
		Update update = new Update.Builder(partialDoc) //
				.index(connection.getIndexName()) //
				.type("feature") //
				.id(_id) //
				.refresh(true) //
				.build();

		try {
			DocumentResult result = getConnection().getJestClient().execute(update);
			if (!result.isSucceeded()) {
				logger.error(result.getErrorMessage());
			}
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
		}

	}

	/** {@inheritDoc} */
	@Override
	public Set<String> readAllGroups() {
		Map<String, Feature> mapOfFeatures = readAll();
		Set<String> groups = new HashSet<String>();
		for (Map.Entry<String, Feature> entry : mapOfFeatures.entrySet()) {
			if (null != entry.getValue().getGroup()) {
				groups.add(entry.getValue().getGroup());
			}
		}
		return groups;
	}

	/** {@inheritDoc} */
	@Override
	public void clear() {
		getConnection().execute(getBuilder().queryClear());
	}

	/** {@inheritDoc} */
	@Override
	public void createSchema() {
		getConnection().execute(getBuilder().queryFlushIndex());
	}

	/**
	 * Getter accessor for attribute 'connection'.
	 *
	 * @return current value of 'connection'
	 */
	public ElasticConnection getConnection() {
		return connection;
	}

	/**
	 * Setter accessor for attribute 'connection'.
	 * 
	 * @param connection
	 *            new value for 'connection '
	 */
	public void setConnection(ElasticConnection connection) {
		this.connection = connection;
	}

	/**
	 * Getter accessor for attribute 'builder'.
	 *
	 * @return current value of 'builder'
	 */
	public ElasticQueryBuilder getBuilder() {
		if (builder == null) {
			builder = new ElasticQueryBuilder(this.connection);
		}
		return builder;
	}
}
