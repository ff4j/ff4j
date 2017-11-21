package org.ff4j.couchbase.store.mapper;

/*
 * #%L
 * ff4j-store-springcouchbase
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.utils.json.FeatureJsonParser;

import java.io.IOException;
import java.util.Map;

public class FlippingStrategyDeserialiser extends StdDeserializer<FlippingStrategy> {
    public FlippingStrategyDeserialiser() {
        this(null);
    }

    public FlippingStrategyDeserialiser(Class<?> vc) {
        super(vc);
    }

    @Override
    public FlippingStrategy deserialize(JsonParser jp, DeserializationContext ctxt) throws IOException {
        JsonNode node = jp.getCodec().readTree(jp);
        return FeatureJsonParser.parseFlipStrategy("", new ObjectMapper().convertValue(node, Map.class));
    }
}