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
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import org.ff4j.property.Property;
import org.ff4j.property.util.PropertyFactory;

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;

public class PropertyDeserialiser extends StdDeserializer<Property> {
    public PropertyDeserialiser() {
        this(null);
    }

    public PropertyDeserialiser(Class<?> vc) {
        super(vc);
    }

    private String parseDate(JsonNode node) {
        long time = node.get("value").asLong();
        Date date = new Date(time);
        return new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(date);
    }

    @Override
    public Property deserialize(JsonParser jp, DeserializationContext ctxt) throws IOException {
        JsonNode node = jp.getCodec().readTree(jp);

        if (!node.has("type")) {
            throw new IllegalArgumentException("Property has no type");
        }

        String name = node.has("name") ? node.get("name").asText() : null;
        String type = node.has("type") ? node.get("type").asText() : null;
        String value = node.has("value") ? node.get("value").asText() : null;
        String description = node.has("description") ? node.get("description").asText() : null;
        Set<String> fixedValues = new HashSet<>();
        if (node.has("fixedValues")) {
            node.get("fixedValues").elements()
                .forEachRemaining(v -> fixedValues.add(v.asText()));
        }

        if (type.equals("org.ff4j.property.PropertyDate")) {
            value = parseDate(node);
        }

        return PropertyFactory.createProperty(name, type, value, description, fixedValues);
    }
}