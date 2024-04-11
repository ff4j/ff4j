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

import java.io.IOException;
import java.net.URL;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.ff4j.core.Feature;
import org.ff4j.elastic.mapper.FeatureMapper;
import org.ff4j.elastic.mapper.PropertyMapper;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.utils.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.GsonBuilder;

import io.searchbox.client.JestClient;
import io.searchbox.client.JestClientFactory;
import io.searchbox.client.JestResult;
import io.searchbox.client.config.HttpClientConfig;
import io.searchbox.core.Search;
import io.searchbox.core.SearchResult;
import io.searchbox.core.SearchResult.Hit;
import io.searchbox.core.Update;
import io.searchbox.indices.CreateIndex;
import io.searchbox.indices.DeleteIndex;
import io.searchbox.indices.Flush;
import io.searchbox.indices.IndicesExists;


/**
 * Wrapper for utilities with JestClient.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class ElasticQueryHelper {
    
    /** Logger for the class. */
    private static final Logger LOGGER = LoggerFactory.getLogger(ElasticQueryHelper.class);
    
    /** Hide default constructor. */
    private ElasticQueryHelper() {}
    
    /**
     * Create a default {@link JestClient} from elastic nodes.
     *
     * @param esNodes
     *      elastic nodes URL
     * @return
     *      {@link JestClient} with default values
     */
    public static JestClient createDefaultJestClient(URL... esNodes) {
        Util.assertParamHasNotNull(esNodes, "Elastic nodes URL list");
        
        // Configuring HttpClient
        HttpClientConfig httpClientConfig = new HttpClientConfig.Builder(
                  Arrays.asList(esNodes).stream()
                        .map(URL::toString).collect(Collectors.toSet()))
                .gson(new GsonBuilder()
                        .registerTypeAdapter(Feature.class, new FeatureMapper())
                        .registerTypeAdapter(Property.class, new PropertyMapper())
                        .create())
                .multiThreaded(true)
                .build();
        
        // Factory with http client configuration
        JestClientFactory factory = new JestClientFactory();
        factory.setHttpClientConfig(httpClientConfig);
        JestClient jestClient = factory.getObject();
        LOGGER.info("Connectivity OK with Elastic");
        return jestClient;
    }
    
    /**
     * Create index if not present.
     */
    public static void createIndexIfNotExist(JestClient jestClient, String indexName) {
        try {
            IndicesExists queryIndiceExist = new IndicesExists.Builder(indexName).build();
            if (!jestClient.execute(queryIndiceExist).isSucceeded()) {
                jestClient.execute(new CreateIndex.Builder(indexName).build());
                LOGGER.info("Index '{}' has been created.", indexName);
            }
        } catch (IOException e) {
            throw new IllegalStateException("Cannot test or create index '" + indexName + "'", e);
        }
    }
    
    /**
     * Drop index if present.
     */
    public static void dropIndexIfExist(JestClient jestClient, String indexName) {
        try {
            IndicesExists queryIndiceExist = new IndicesExists.Builder(indexName).build();
            if (jestClient.execute(queryIndiceExist).isSucceeded()) {
                jestClient.execute(new DeleteIndex.Builder(indexName).build());
                LOGGER.info("Index '{}' has been deleted.", indexName);
            }
        } catch (IOException e) {
            throw new IllegalStateException("Cannot test or create index '" + indexName + "'", e);
        }
    }
    
    /**
     * Flush index (transactionlog -> lucene)
     */
    public static void flushIndex(JestClient jestClient, String indexName) {
        try {
            jestClient.execute(new Flush.Builder().addIndex(indexName).build());
            LOGGER.info("Index '{}' has been flushed.", indexName);
        } catch (IOException e) {
            throw new IllegalStateException("Cannot flush index '" + indexName + "'", e);
        }
    }
    
    /** Read internal object id from function feature uid.*/
    @SuppressWarnings("rawtypes")
    public static String findFeatureTechIdFromUid(JestClient jestClient, String indexFeatures, String uid) {
        try {
            Search search = ElasticQueryBuilder.findFeatureByUid(indexFeatures, uid);
            List<Hit<Map, Void>> items = jestClient.execute(search).getHits(Map.class);
            if (items == null || items.isEmpty()) {
                throw new FeatureNotFoundException("Cannot find feature '" + uid + "'");
            }
            return items.get(0).source.get(JestResult.ES_METADATA_ID).toString();
        } catch (IOException e) {
            throw new IllegalStateException("Error during mapping uid/_id for '" + uid + "'", e);
        }
    }
    
    /** Read internal object id from function event uid.*/
    @SuppressWarnings("rawtypes")
    public static String findEventTechIdFromUid(JestClient jestClient, String indexEvent, String uid) {
        try {
            Search search = ElasticQueryBuilder.findEventById(indexEvent, uid);
            List<Hit<Map, Void>> items = jestClient.execute(search).getHits(Map.class);
            if (items == null || items.isEmpty()) {
                throw new FeatureNotFoundException("Cannot find event '" + uid + "'");
            }
            return items.get(0).source.get(JestResult.ES_METADATA_ID).toString();
        } catch (IOException e) {
            throw new IllegalStateException("Error during mapping uid/_id for '" + uid + "'", e);
        }
    }
    
    /** Read internal object id from function property name.*/
    @SuppressWarnings("rawtypes")
    public static String findPropertyTechIdFromName(JestClient jestClient, String indexProperties, String name) {
        try {
            Search search = ElasticQueryBuilder.findPropertyByName(indexProperties, name);
            List<Hit<Map, Void>> items = jestClient.execute(search).getHits(Map.class);
            if (items == null || items.isEmpty()) {
                throw new FeatureNotFoundException("Cannot find property '" + name + "'");
            }
            return items.get(0).source.get(JestResult.ES_METADATA_ID).toString();
        } catch (IOException e) {
            throw new IllegalStateException("Error during mapping name/_id for property '" + name + "'", e);
        }
    }
    
    @SuppressWarnings({ "rawtypes" })
    public static Set<String> findFeatureTechIdsFromGroupName(JestClient jestClient, String indexFeatures, String groupName) {
        try {
            Search search = ElasticQueryBuilder.findFeaturesByGroupName(indexFeatures, groupName);
            SearchResult result = jestClient.execute(search);
            Set<String> metadatas = new HashSet<String>();
            if (null != result && result.isSucceeded()) {
                List<Hit<Map, Void>> features = result.getHits(Map.class);
                if (features != null && !features.isEmpty()) {
                    for (Hit<Map, Void> hit : features) {
                        metadatas.add(hit.source.get(JestResult.ES_METADATA_ID).toString());
                    }
                }
            }            
            return metadatas;
        } catch (IOException e) {
            throw new IllegalStateException("Error during mapping groupName/_id for '" + groupName + "'", e);
        }
    }

    /**
     * Toggle on/offS.
     */
    public static void toggle(JestClient jestClient, String indexFeatures, String uid, boolean enable) {
        try {
            LOGGER.debug("Toggle '{}' feature '{}'", enable, uid);
            String techid = findFeatureTechIdFromUid(jestClient, indexFeatures, uid);
            Update update = ElasticQueryBuilder.toggle(indexFeatures, techid, enable);
            jestClient.execute(update);
        } catch (IOException e) {
            throw new IllegalStateException("Error during toggling '" + uid + "'", e);
        }
    }
}
