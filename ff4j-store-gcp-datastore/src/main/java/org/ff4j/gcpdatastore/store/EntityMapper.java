package org.ff4j.gcpdatastore.store;

/*
 * #%L
 * ff4j-store-gcp-datastore
 * %%
 * Copyright (C) 2013 - 2022 FF4J
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


import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.cloud.datastore.Entity;
import com.google.cloud.datastore.Key;
import com.google.cloud.datastore.KeyFactory;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.gcpdatastore.store.feature.DatastoreFeature;
import org.ff4j.gcpdatastore.store.property.DatastoreProperty;
import org.ff4j.utils.JsonUtils;
import org.ff4j.utils.json.FeatureJsonParser;

import java.util.Collections;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import static org.ff4j.gcpdatastore.store.feature.DatastoreFeature.*;
import static org.ff4j.gcpdatastore.store.property.DatastoreProperty.*;

public class EntityMapper {

    private static ObjectMapper objectMapper = new ObjectMapper();

    static {
        objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
    }

    public static Entity toEntity(DatastoreFeature f, KeyFactory kf) {
        String uid = f.getUid();
        Key key = kf.newKey(uid);
        String description = Optional.ofNullable(f.getDescription()).orElse("");
        String group = Optional.ofNullable(f.getGroup()).orElse("");
        String permissions = JsonUtils.collectionAsJson(Optional.ofNullable(f.getPermissions()).orElse(Collections.emptySet()));
        String flippingStrategy = JsonUtils.flippingStrategyAsJson(Optional.ofNullable(f.getFlippingStrategy()).orElse(null));
        String customProperties = customPropertiesAsJson(Optional.ofNullable(f.getCustomProperties()).orElse(Collections.emptyMap()));

        return Entity.newBuilder(key)
                .set(FEATURE_UUID, uid)
                .set(FEATURE_ENABLE, f.isEnable())
                .set(FEATURE_DESCRIPTION, description)
                .set(FEATURE_GROUP, group)
                .set(FEATURE_PERMISSIONS, permissions)
                .set(FEATURE_FLIPPING_STRATEGY, flippingStrategy)
                .set(FEATURE_CUSTOM_PROPERTIES, customProperties)
                .build();
    }

    public static DatastoreFeature fromEntity(Entity e) {
        String id = e.getKey().getName();
        String uid = e.getString(FEATURE_UUID);
        boolean isEnable = e.getBoolean(FEATURE_ENABLE);
        String description = e.getString(FEATURE_DESCRIPTION);
        String group = e.getString(FEATURE_GROUP);
        Set<String> permissions = FeatureJsonParser.parsePermissions(e.getString(FEATURE_PERMISSIONS));
        FlippingStrategy flippingStrategy = FeatureJsonParser.parseFlipStrategyAsJson(e.getString(FEATURE_UUID), e.getString(FEATURE_FLIPPING_STRATEGY));
        Map<String, DatastoreProperty> customProperties = parseCustomPropertiesFromJson(e.getString(FEATURE_CUSTOM_PROPERTIES));

        return DatastoreFeature.builder()
                .uid(uid)
                .enable(isEnable)
                .description(description)
                .group(group)
                .permissions(permissions)
                .flippingStrategy(flippingStrategy)
                .customProperties(customProperties)
                .build();
    }

    public static Entity toEntity(DatastoreProperty p, KeyFactory kf) {
        String id = p.getId();
        Key key = kf.newKey(id);
        String name = Optional.ofNullable(p.getName()).orElse("");
        String description = Optional.ofNullable(p.getDescription()).orElse("");
        String type = Optional.ofNullable(p.getType()).orElse("");
        String value = Optional.ofNullable(p.getValue()).orElse("");
        String fixedValues = JsonUtils.collectionAsJson(Optional.ofNullable(p.getFixedValues()).orElse(Collections.emptySet()));

        return Entity.newBuilder(key)
                .set(PROPERTY_ID, id)
                .set(PROPERTY_READONLY, p.isReadOnly())
                .set(PROPERTY_NAME, name)
                .set(PROPERTY_DESCRIPTION, description)
                .set(PROPERTY_TYPE, type)
                .set(PROPERTY_VALUE, value)
                .set(PROPERTY_FIXED_VALUES, fixedValues)
                .build();
    }

    public static DatastoreProperty fromPropertyEntity(Entity e) {
        String id = e.getKey().getName();
        boolean isReadOnly = e.getBoolean(PROPERTY_READONLY);
        String name = e.getString(PROPERTY_NAME);
        String description = e.getString(PROPERTY_DESCRIPTION);
        String type = e.getString(PROPERTY_TYPE);
        String value = e.getString(PROPERTY_VALUE);
        Set<String> fixedValues = parseSet(e.getString(PROPERTY_FIXED_VALUES));

        return DatastoreProperty.builder()
                .id(id)
                .readOnly(isReadOnly)
                .name(name)
                .description(description)
                .type(type)
                .value(value)
                .fixedValues(fixedValues)
                .build();
    }

    private static String customPropertiesAsJson(Map<String, DatastoreProperty> cp) {
        try {
            return objectMapper.writeValueAsString(cp);
        } catch (JsonProcessingException e) {
            throw new IllegalArgumentException("Can't marshal customProperties", e);
        }
    }

    private static Map<String, DatastoreProperty> parseCustomPropertiesFromJson(String cp) {
        TypeReference<Map<String, DatastoreProperty>> typeRef = new TypeReference<Map<String, DatastoreProperty>>() {
        };
        try {
            return objectMapper.readValue(cp, typeRef);
        } catch (JsonProcessingException e) {
            throw new IllegalArgumentException("Can't unmarshal customProperties", e);
        }
    }

    private static Set<String> parseSet(String json) {
        if (json == null) return null;
        try {
            return objectMapper.readValue(json, Set.class);
        } catch (JsonProcessingException e) {
            throw new IllegalArgumentException("Can't unmarshal set", e);
        }
    }
}
