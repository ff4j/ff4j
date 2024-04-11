package org.ff4j.elastic;

/*-
 * #%L
 * ff4j-store-elastic
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

import java.util.Set;

import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.elasticsearch.index.query.RangeQueryBuilder;
import org.elasticsearch.search.builder.SearchSourceBuilder;
import org.ff4j.audit.Event;
import org.ff4j.audit.EventConstants;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.core.Feature;
import org.ff4j.elastic.mapper.FeatureMapper;
import org.ff4j.elastic.mapper.PropertyMapper;
import org.ff4j.property.Property;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.searchbox.core.Delete;
import io.searchbox.core.DeleteByQuery;
import io.searchbox.core.Index;
import io.searchbox.core.Search;
import io.searchbox.core.Update;

/**
 * Helper to create Jest queries.
 *
 * @author Cedrick LUNVEN (@clunven)
 * @author Andre BLASZCZYK (andre.blaszczyk@gmail.com)
 */
public class ElasticQueryBuilder {
	
    public static final String TYPE_EVENT = "event";
    
    /** Logger for the class. */
    private static final Logger LOGGER = LoggerFactory.getLogger(ElasticQueryBuilder.class);
    
    /**
     * Hide Default constructor.
     */
    private ElasticQueryBuilder() {}
    
    // ----- Features -----
    
    /** Update property 'enable' in doc feature */
    public static Update toggle(String indexFeatures, String techid, boolean enable) {
        String partialDoc = "{ \"doc\" : { \"enable\" : " + enable + " } }";
        return new Update.Builder(partialDoc)
                .index(indexFeatures)
                .type(FeatureMapper.TYPE_FEATURE)
                .id(techid)
                .refresh(true)
                .build();
    }
    
	/** Read a feature. */
	public static Search findFeatureByUid(String indexFeatures, String uid) {
		SearchSourceBuilder source = new SearchSourceBuilder();
		source.query(QueryBuilders.matchQuery(FeatureMapper.FEATURE_UID, uid));
		return new Search.Builder(source.toString()) 
		        .addIndex(indexFeatures)
                .build();
	}

	/** Read a group. */
	public static Search findGroupByGroupName(String indexFeatures, String groupName) {
		SearchSourceBuilder searchSourceBuilder = new SearchSourceBuilder();
		searchSourceBuilder.query(QueryBuilders.matchQuery(FeatureMapper.FEATURE_GROUP, groupName));
		LOGGER.debug("[findGroupByGroupName] with '{}' query:{}", searchSourceBuilder.toString());
        Search search = new Search.Builder(searchSourceBuilder.toString())
		        .addIndex(indexFeatures)
                .addType(FeatureMapper.TYPE_FEATURE)
                .build();
		return search;
	}

	/** Insert a feature. */
	public static Index createFeature(String indexFeatures, Feature fp) {
		return new Index.Builder(fp)
		        .index(indexFeatures)
		        .type(FeatureMapper.TYPE_FEATURE)
		        .refresh(true)
				.build();
	}

	/** Delete a feature. */
	public static Delete deleteFeature(String indexFeatures, String techid, String uid) {
        return new Delete
                .Builder(uid)
                .index(indexFeatures)
                .type(FeatureMapper.TYPE_FEATURE)
                .id(techid)
                .refresh(true)
                .build();
    }

	public static DeleteByQuery deleteAllFeatures(String indexFeatures) {
	    String queryDeleteAll = "{ \"query\": { \"match_all\" : { }  } }";
	     return new DeleteByQuery.Builder(queryDeleteAll)
                .addIndex(indexFeatures)
                .addType(FeatureMapper.TYPE_FEATURE)
                .refresh(true)
                .build();
	}
	
	/** Update a feature. */
    public static Update updateFeature(String indexFeatures, String techid, Feature fp) {
        return new Update.Builder(fp)
                .type(FeatureMapper.TYPE_FEATURE)
                .id(techid)
                .refresh(true)
                .build();
    }
	
	/** Find all features. */
	public static Search findAllFeatures(String indexFeatures) {
		return new Search
		        .Builder(new SearchSourceBuilder().toString())
		        .addIndex(indexFeatures)
                .addType(FeatureMapper.TYPE_FEATURE)
                .build();
	}
	
	/** Find all features with a limit. */
    public static Search findAllFeaturesLimit(String indexFeatures, int limit) {
        return new Search
                .Builder(new SearchSourceBuilder().size(limit).toString())
                .addIndex(indexFeatures)
                .addType(FeatureMapper.TYPE_FEATURE)
                .build();
    }
    
    /** Find technical ids of features in a group. */
    public static Search findFeaturesByGroupName(String indexFeatures, String groupName) {
        SearchSourceBuilder searchSourceBuilder = new SearchSourceBuilder();
        searchSourceBuilder.query(QueryBuilders.matchQuery(FeatureMapper.FEATURE_GROUP, groupName));
        return new Search.Builder(searchSourceBuilder.toString())
                .addIndex(indexFeatures)
                .addType(FeatureMapper.TYPE_FEATURE)
                .build();
    }
    
    /** Add a feature to a group. */
    public static Update updateFeatureAddToGroup(String indexFeatures, String techid, String groupName) {
        return new Update.Builder("{ \"doc\" : { \"group\" : \"" + groupName + "\" } }")
                .index(indexFeatures)
                .type(FeatureMapper.TYPE_FEATURE)
                .id(techid)
                .build();
    }
    
    /** Remove a feature from a group. */
    public static Update updateFeatureRemoveFromGroup(String indexFeatures, String techid) {
        return new Update.Builder("{ \"doc\" : { \"group\" : \"\" } }")
                .index(indexFeatures)
                .type(FeatureMapper.TYPE_FEATURE)
                .id(techid)
                .build();
    }
    
    // ----- Properties -----

    public static DeleteByQuery deleteAllProperties(String indexProperties) {
        String queryDeleteAll = "{ \"query\": { \"match_all\" : { }  } }";
         return new DeleteByQuery.Builder(queryDeleteAll)
                .addIndex(indexProperties)
                .addType(PropertyMapper.TYPE_PROPERTY)
                .refresh(true)
                .build();
    }
    
	public static Search findAllProperties(String indexProperties) {
	    return new Search.Builder(new SearchSourceBuilder().toString())
                .addIndex(indexProperties)
                .addType(PropertyMapper.TYPE_PROPERTY)
                .build();	
	}

	/** Find all features with a limit. */
    public static Search findAllPropertiesLimit(String indexProperties, int limit) {
        return new Search.Builder(new SearchSourceBuilder().size(limit).toString())
                .addIndex(indexProperties)
                .addType(PropertyMapper.TYPE_PROPERTY)
                .build();   
    }

    /** Find property by its name. */
	public static Search findPropertyByName(String indexProperties, String name) {
		SearchSourceBuilder source = new SearchSourceBuilder();
		source.query(QueryBuilders.matchQuery(PropertyMapper.PROPERTY_NAME, name));
		return new Search.Builder(source.toString())
		        .addIndex(indexProperties)
                .addType(PropertyMapper.TYPE_PROPERTY)
                .build();
	}

	/** Insert a feature. */
    public static Index createProperty(String indexProperties, Property<?> property) {
        return new Index.Builder(property)
                .index(indexProperties)
                .type(PropertyMapper.TYPE_PROPERTY)
                .refresh(true)
                .build();
    }
    
    /** Delete a feature. */
    public static Delete deleteProperty(String indexProperties, String techid, String name) {
        return new Delete
                .Builder(name)
                .index(indexProperties)
                .type(PropertyMapper.TYPE_PROPERTY)
                .id(techid)
                .refresh(true)
                .build();
    }

    // ----- Events -----
    
    /** Insert a feature. */
    public static Index createEvent(String indexEvents, Event event) {
        return new Index.Builder(event)
                .index(indexEvents)
                .type(TYPE_EVENT)
                .refresh(true)
                .build();
    }
    
    /** Read an event. */
    public static Search findEventById(String indexEvents, String uuid) {
        SearchSourceBuilder source = new SearchSourceBuilder();
        source.query(QueryBuilders.matchQuery("uuid", uuid));
        return new Search.Builder(source.toString()) 
                .addIndex(indexEvents)
                .addType(TYPE_EVENT)
                .build();
    }

	public static Search findEventsFromQueryDefinition(String indexEvents, EventQueryDefinition query, String action) {
		BoolQueryBuilder booleanQuery = new BoolQueryBuilder();
		// Optional constant for action filter
		if (action != null) {
			query.getActionFilters().add(action);
		}
		QueryBuilder typeQuery = QueryBuilders.termQuery("type", EventConstants.TARGET_FEATURE);
		RangeQueryBuilder timestampFilter = QueryBuilders.rangeQuery("timestamp") //
				.gt(query.getFrom().longValue()) //
				.lt(query.getTo().longValue()) //
				.includeLower(false) //
				.includeUpper(false);

		booleanQuery.must(typeQuery);
		booleanQuery.must(timestampFilter);

		// Optional filters
		addOptionalFilters(booleanQuery, query.getActionFilters(), "action");
		addOptionalFilters(booleanQuery, query.getHostFilters(),   "hostName");
		addOptionalFilters(booleanQuery, query.getNamesFilter(),   "name");
		addOptionalFilters(booleanQuery, query.getSourceFilters(), "source");

		// Warning : default size is set to 10 results, that's why it's
		// overridden
		SearchSourceBuilder searchSourceBuilder = new SearchSourceBuilder().size(100);
		return new Search.Builder(searchSourceBuilder.query(booleanQuery).toString())
		        .addIndex(indexEvents)
                .addType(TYPE_EVENT)
                .build();
	}
	
	/** Delete a feature. */
    public static Delete deleteEvent(String indexEvent, String techid, String uid) {
        return new Delete
                .Builder(uid)
                .index(indexEvent)
                .type(TYPE_EVENT)
                .id(techid)
                .refresh(true)
                .build();
    }
    
    public static DeleteByQuery deleteAllEvents(String indexEvent) {
        String queryDeleteAll = "{ \"query\": { \"match_all\" : { }  } }";
         return new DeleteByQuery.Builder(queryDeleteAll)
                .addIndex(indexEvent)
                .addType(TYPE_EVENT)
                .refresh(true)
                .build();
    }

	private static void addOptionalFilters(BoolQueryBuilder booleanQuery, Set<String> filters, String field) {
		if (!filters.isEmpty()) {
			BoolQueryBuilder subQuery = new BoolQueryBuilder();
			for (String filter : filters) {
				subQuery.must(QueryBuilders.matchQuery(field, filter));
			}
			booleanQuery.must(subQuery);
		}
	}

	/** Find all features. */
    public static Search findAllEvents(String indexEvents) {
        return new Search
                .Builder(new SearchSourceBuilder().toString())
                .addIndex(indexEvents)
                .addType(TYPE_EVENT)
                .build();
    }
    
}
