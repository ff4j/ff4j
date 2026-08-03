package org.ff4j.elastic;

/*-
 * #%L
 * ff4j-store-elastic
 * %%
 * Copyright (C) 2013 - 2026 FF4J
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

import static org.ff4j.elastic.ElasticQueryBuilder.MAX_RESULT_SIZE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import com.google.gson.Gson;
import com.google.gson.JsonObject;

import io.searchbox.core.Search;

/**
 * Elasticsearch returns only 10 hits per search unless an explicit size is set:
 * the "find all" queries must override it or stores silently truncate results (issue #732).
 */
public class ElasticQueryBuilderTest {

    private static final Gson GSON = new Gson();

    private int sizeOf(Search search) {
        JsonObject payload = GSON.fromJson(search.getData(GSON), JsonObject.class);
        assertTrue(payload.has("size"), "Query must override Elasticsearch default size (10): " + payload);
        return payload.get("size").getAsInt();
    }

    @Test
    public void findAllQueriesShouldOverrideDefaultSize() {
        assertEquals(MAX_RESULT_SIZE, sizeOf(ElasticQueryBuilder.findAllFeatures("ff4j_features")));
        assertEquals(MAX_RESULT_SIZE, sizeOf(ElasticQueryBuilder.findAllProperties("ff4j_properties")));
        assertEquals(MAX_RESULT_SIZE, sizeOf(ElasticQueryBuilder.findAllEvents("ff4j_events")));
        assertEquals(MAX_RESULT_SIZE, sizeOf(ElasticQueryBuilder.findFeaturesByGroupName("ff4j_features", "g1")));
        assertEquals(MAX_RESULT_SIZE, sizeOf(ElasticQueryBuilder.findGroupByGroupName("ff4j_features", "g1")));
    }

    @Test
    public void findAllLimitQueriesShouldUseProvidedLimit() {
        assertEquals(50, sizeOf(ElasticQueryBuilder.findAllFeaturesLimit("ff4j_features", 50)));
        assertEquals(50, sizeOf(ElasticQueryBuilder.findAllPropertiesLimit("ff4j_properties", 50)));
    }
}
