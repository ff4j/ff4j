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
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.store.AbstractFeatureStore;
import org.ff4j.utils.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.searchbox.client.JestResult;
import io.searchbox.core.Delete;
import io.searchbox.core.DocumentResult;
import io.searchbox.core.Index;
import io.searchbox.core.Search;
import io.searchbox.core.SearchResult;
import io.searchbox.core.SearchResult.Hit;
import io.searchbox.core.Update;
import io.searchbox.indices.DeleteIndex;
import io.searchbox.indices.Flush;

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
		
		try {
		    importFeaturesFromXmlFile(xmlFile);
	        Flush flush = new Flush.Builder().addIndex(connection.getIndexName()).build();
	        getConnection().getJestClient().execute(flush);
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw new FeatureAccessException("Cannot enforce index clearing", e);
		}
	}

	/** {@inheritDoc} */
	@Override
	public void enable(String uid) {
		this.updateStatus(uid, true);
	}

	/** {@inheritDoc} */
	@Override
	public void disable(String uid) {
		this.updateStatus(uid, false);
	}

	/** {@inheritDoc} */
	@Override
	public boolean exist(String featureID) {
		// Checking featureID is not null
		Util.assertHasLength(featureID);
		try {
		    // Jest request
	        SearchSourceBuilder searchSourceBuilder = new SearchSourceBuilder();
	        searchSourceBuilder.query(QueryBuilders.matchQuery("uid", featureID));
	        Search search = new Search.Builder(searchSourceBuilder.toString()) //
	                .addIndex(connection.getIndexName()) //
	                .addType("feature") //
	                .build();
	        
			SearchResult result = getConnection().getJestClient().execute(search);
			
			return (null != result) && result.isSucceeded() &&
			       (result.getTotal() != null) && (result.getTotal() >= 1);
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw new FeatureAccessException("Cannot check feature existence", e);
		}
	}

	/** {@inheritDoc} */
	@Override
	public void create(Feature fp) {
	    // Checking parameter
        assertFeatureNotNull(fp);
	    assertFeatureNotExist(fp.getUid());
	    
		try {
		    
		    // Jest request
	        Index index = new Index.Builder(fp) //
	                .index(connection.getIndexName()) //
	                .type("feature")//
	                .refresh(true) //
	                .build();
	        
			DocumentResult result = getConnection().getJestClient().execute(index);
			if (!result.isSucceeded()) {
				logger.error(result.getErrorMessage());
				throw new FeatureAccessException("Cannot create feature " + fp.getUid() + " query failed :" + result.getErrorMessage());
			}
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
			throw new FeatureAccessException("Cannot create feature " + fp.getUid() + " an exception occured", e);
		}
	}

	/** {@inheritDoc} */
	@Override
	public Feature read(String featureUid) {
        // Checking parameter
	    Util.assertHasLength(featureUid);
		SearchResult result;
		
		try {
		    // Jest request
	        SearchSourceBuilder searchSourceBuilder = new SearchSourceBuilder();
	        searchSourceBuilder.query(QueryBuilders.matchQuery("uid", featureUid));
	        Search search = new Search.Builder(searchSourceBuilder.toString()) //
	                .addIndex(connection.getIndexName()) //
	                .addType("feature") //
	                .build();

			result = getConnection().getJestClient().execute(search);
			if (!result.isSucceeded()) {
				logger.error(result.getErrorMessage());
			}
			if (result.getTotal() == 0) {
				throw new FeatureNotFoundException(featureUid);
			}
		} catch (Exception e) {
			throw new FeatureNotFoundException(featureUid);
		}

		return result.getFirstHit(Feature.class).source;
	}

	/** {@inheritDoc} */
	@Override
	public Map<String, Feature> readAll() {

		// Jest request
		SearchSourceBuilder searchSourceBuilder = new SearchSourceBuilder();
		Search search = new Search.Builder(searchSourceBuilder.toString()) //
				.addIndex(connection.getIndexName()) //
				.addType("feature") //
				.build();

		Map<String, Feature> mapFP = new HashMap<String, Feature>();
		SearchResult result = null;
		try {
			result = getConnection().getJestClient().execute(search);
			if (!result.isSucceeded()) {
				logger.error(result.getErrorMessage());
			}
			List<Hit<Feature, Void>> features = result.getHits(Feature.class);
			for (Hit<Feature, Void> feature : features) {
				mapFP.put(feature.source.getUid(), feature.source);
			}
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
		}

		return mapFP;
	}

	/** {@inheritDoc} */
	@Override
	public void delete(String fpId) {

		// Checking parameter
		if (fpId == null || fpId.isEmpty()) {
			throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
		}

		// Getting metadata
		String _id = getMetaDataId(fpId);
		if (_id == null) {
			throw new FeatureNotFoundException(fpId);
		}

		try {
			// Jest request
			Delete delete = new Delete.Builder(fpId) //
					.index(connection.getIndexName()) //
					.type("feature") //
					.id(_id) //
					.refresh(true) //
					.build();

			DocumentResult result = getConnection().getJestClient().execute(delete);
			if (!result.isSucceeded()) {
				logger.error(result.getErrorMessage());
			}
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
		}
	}

	/** {@inheritDoc} */
	@Override
	public void update(Feature fp) {

		// Checking parameter
		if (fp == null) {
			throw new IllegalArgumentException("Feature cannot be null nor empty");
		}

		// Getting metadata
		String _id = getMetaDataId(fp.getUid());
		if (_id == null) {
			throw new FeatureNotFoundException(fp.getUid());
		}

		// Jest request
		Index update = new Index.Builder(fp) //
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
	public void grantRoleOnFeature(String flipId, String roleName) {

		// Checking parameter
		if (flipId == null || flipId.isEmpty()) {
			throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
		}

		if (roleName == null || roleName.isEmpty()) {
			throw new IllegalArgumentException("Role cannot be null nor empty");
		}

		// Getting metadata
		String _id = getMetaDataId(flipId);
		if (_id == null) {
			throw new FeatureNotFoundException(flipId);
		}

		// Updating feature with role
		Feature feature = read(flipId);
		feature.getPermissions().add(roleName);

		// Jest request
		Index update = new Index.Builder(feature) //
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
	public void removeRoleFromFeature(String flipId, String roleName) {

		// Checking parameter
		if (flipId == null || flipId.isEmpty()) {
			throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
		}

		if (roleName == null || roleName.isEmpty()) {
			throw new IllegalArgumentException("Role cannot be null nor empty");
		}

		// Getting metadata
		String _id = getMetaDataId(flipId);
		if (_id == null) {
			throw new FeatureNotFoundException(flipId);
		}

		// Updating feature with role
		Feature feature = read(flipId);
		feature.getPermissions().remove(roleName);

		// Jest request
		Index update = new Index.Builder(feature) //
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
	public void enableGroup(String groupName) {

		// Checking parameter
		if (groupName == null || groupName.isEmpty()) {
			throw new IllegalArgumentException("Groupname cannot be null nor empty");
		}
		if (!existGroup(groupName)) {
			throw new GroupNotFoundException(groupName);
		}

		// Getting all feature metadata according to this group
		Set<String> metadatas = findMetadataByGroup(groupName);

		// Partial group update
		String partialDoc = "{\n" + //
				"    \"doc\" : {\n" + //
				"        \"enable\" : true\n" + //
				"    }\n" + //
				"}"; //

		// Jest request
		try {
			for (String _id : metadatas) {
				Update update = new Update.Builder(partialDoc) //
						.index(connection.getIndexName()) //
						.type("feature") //
						.id(_id) //
						.build();
				DocumentResult result = getConnection().getJestClient().execute(update);
				if (!result.isSucceeded()) {
					logger.error(result.getErrorMessage());
				}
			}
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
		}
	}

	/** {@inheritDoc} */
	@Override
	public void disableGroup(String groupName) {
	    // Controls
	    Util.assertHasLength(groupName);
		if (!existGroup(groupName)) {
			throw new GroupNotFoundException(groupName);
		}

		// Getting all feature metadata according to this group
		Set<String> metadatas = findMetadataByGroup(groupName);
		
		// Partial group update
		String partialDoc = "{ \"doc\" : { \"enable\" : false } }";

		// Jest request
		try {
			for (String _id : metadatas) {
				Update update = new Update.Builder(partialDoc) //
						.index(connection.getIndexName()) //
						.type("feature") //
						.id(_id) //
						.refresh(true) //
						.build();
				DocumentResult result = getConnection().getJestClient().execute(update);
				if (!result.isSucceeded()) {
					logger.error(result.getErrorMessage());
				}
			}
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
		}
	}

	/** {@inheritDoc} */
	@Override
	public boolean existGroup(String groupName) {

		// Checking parameter
		if (groupName == null || groupName.isEmpty()) {
			throw new IllegalArgumentException("Groupname cannot be null nor empty");
		}

		// Jest request
		SearchSourceBuilder searchSourceBuilder = new SearchSourceBuilder();
		searchSourceBuilder.query(QueryBuilders.matchQuery("group", groupName));
		Search search = new Search.Builder(searchSourceBuilder.toString()) //
				.addIndex(connection.getIndexName()) //
				.addType("feature") //
				.build();

		try {
			SearchResult result = getConnection().getJestClient().execute(search);
			if (!result.isSucceeded()) {
				logger.error(result.getErrorMessage());
			}
			return result.getTotal() >= 1;
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
		}

		return false;
	}

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
		String _id = getMetaDataId(featureId);
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
		String _id = getMetaDataId(featureId);
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
	    Map <String, Feature> mapOfFeatures = readAll();
	    Set < String > groups = new HashSet<String>();
	    for (Map.Entry<String, Feature> entry : mapOfFeatures.entrySet()) {
	        if (null != entry.getValue().getGroup()) {
	            groups.add(entry.getValue().getGroup());
	        }
        }

	    		// Jest request
//		SearchSourceBuilder searchSourceBuilder = new SearchSourceBuilder();
//		searchSourceBuilder.query(FilterBuilders.boolFilter().must(FilterBuilders.existsFilter("group")).toString());
//
//		Search search = new Search.Builder(searchSourceBuilder.toString()) //
//				.addIndex(connection.getIndexName()) //
//				.addType("feature") //
//				.refresh(true) //
//				.build();
//
//		Set<String> groups = new HashSet<String>();
//		try {
//			SearchResult result = getConnection().getJestClient().execute(search);
//			if (!result.isSucceeded()) {
//				logger.error(result.getErrorMessage());
//			} else {
//				List<Hit<Feature, Void>> features = result.getHits(Feature.class);
//				for (Hit<Feature, Void> feature : features) {
//					groups.add(feature.source.getGroup());
//				}
//			}
//		} catch (IOException e) {
//			logger.error(e.getMessage(), e);
//		}

		return groups;
	}

	/** {@inheritDoc} */
	@Override
	public void clear() {
		try {
			DeleteIndex delete = new DeleteIndex.Builder(connection.getIndexName()).build();
			JestResult result = getConnection().getJestClient().execute(delete);
			if (!result.isSucceeded()) {
				logger.error(result.getErrorMessage());
			}
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
		}
	}

	/** {@inheritDoc} */
	@Override
	public void createSchema() {
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

	// Convenient methods

	private String getMetaDataId(String featureUID) {

		// No featureUID, then no Metadata ID.
		if (featureUID == null || featureUID.isEmpty()) {
			return null;
		}

		// Jest request
		SearchSourceBuilder searchSourceBuilder = new SearchSourceBuilder();
		searchSourceBuilder.query(QueryBuilders.matchQuery("uid", featureUID));
		Search search = new Search.Builder(searchSourceBuilder.toString()) //
				.addIndex(connection.getIndexName()) //
				.addType("feature") //
				.build();

		String _id = null;
		try {
			_id = getConnection().getJestClient().execute(search) //
					.getHits(Map.class).get(0) //
							.source.get(JestResult.ES_METADATA_ID)//
									.toString();
		} catch (Exception e) {
			throw new FeatureNotFoundException(featureUID);
		}

		return _id;
	}

	@SuppressWarnings({"rawtypes"})
    public Set<String> findMetadataByGroup(String groupName) {
	    if (!existGroup(groupName)) {
			throw new GroupNotFoundException(groupName);
		}		

		Set<String> metadatas = new HashSet<String>();
		try {
		    // Jest request
	        SearchSourceBuilder searchSourceBuilder = new SearchSourceBuilder();
	        searchSourceBuilder.query(QueryBuilders.matchQuery("group", groupName));
	        Search search = new Search.Builder(searchSourceBuilder.toString()) //
	                .addIndex(connection.getIndexName()) //
	                .addType("feature") //
	                .build();
	        
			SearchResult result = getConnection().getJestClient().execute(search);
			if (!result.isSucceeded()) {
				logger.error(result.getErrorMessage());
			}
			List<Hit<Map, Void>> features = result.getHits(Map.class);
			for (Hit<Map, Void> hit : features) {
			    System.out.println(hit);
			    metadatas.add(hit.source.get(JestResult.ES_METADATA_ID).toString());
				//metadatas.add(features.get(0).source.get(JestResult.ES_METADATA_ID).toString());
			}
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
		}
		return metadatas;
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

		// Checking input parameters
		Util.assertParamHasLength(uid, "uid (feature identifier)");

		// Getting metadata id
		String _id = getMetaDataId(uid);
		if (_id == null) {
			throw new FeatureNotFoundException(uid);
		}

		// Partial feature update with metadata id as identifier.
		String partialDoc = "{\n" + //
				"    \"doc\" : {\n" + //
				"        \"enable\" : \"" + enable + "\"\n" + //
				"    }\n" + //
				"  }\n" + //
				"}"; //

		Update update = new Update.Builder(partialDoc) //
				.index(connection.getIndexName()) //
				.type("feature") //
				.id(_id) //
				.refresh(true) //
				.build();

		try {
			getConnection().getJestClient().execute(update);
		} catch (Exception e) {
			logger.error(e.getMessage(), e);
		}
	}
}
