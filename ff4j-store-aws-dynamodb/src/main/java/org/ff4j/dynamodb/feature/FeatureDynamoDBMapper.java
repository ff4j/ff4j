package org.ff4j.dynamodb.feature;


/*
 * #%L
 * ff4j-store-aws-dynamodb
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


import com.amazonaws.services.dynamodbv2.document.Item;
import com.amazonaws.util.CollectionUtils;
import com.amazonaws.util.StringUtils;
import org.ff4j.core.Feature;
import org.ff4j.mapper.FeatureMapper;
import org.ff4j.property.Property;
import org.ff4j.utils.JsonUtils;
import org.ff4j.utils.Util;
import org.ff4j.utils.json.FeatureJsonParser;
import org.ff4j.utils.json.PropertyJsonParser;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static org.ff4j.dynamodb.DynamoDBConstants.*;

/**
 * Implementation of {@link FeatureMapper} for DynamoDB store
 *
 * @author <a href="mailto:jeromevdl@gmail.com">Jerome VAN DER LINDEN</a>
 */
public class FeatureDynamoDBMapper implements FeatureMapper<Item> {

    @Override
    public Item toStore(Feature feature) {
        Item item = new Item();
        item.withString(FEATURE_UID, feature.getUid())
                .withBoolean(FEATURE_ENABLE, feature.isEnable());

        if (!StringUtils.isNullOrEmpty(feature.getDescription())) {
            item.withString(FEATURE_DESCRIPTION, feature.getDescription());
        }

        if (!StringUtils.isNullOrEmpty(feature.getGroup())) {
            item.withString(FEATURE_GROUP, feature.getGroup());
        }

        if (feature.getFlippingStrategy() != null) {
            item.withJSON(FEATURE_STRATEGY, JsonUtils.flippingStrategyAsJson(feature.getFlippingStrategy()));
        }

        if (!CollectionUtils.isNullOrEmpty(feature.getPermissions())) {
            // DynamoDB does not support empty strings in a Set
            feature.getPermissions().remove("");
            if (!CollectionUtils.isNullOrEmpty(feature.getPermissions())) {
                item.withStringSet(FEATURE_ROLE, feature.getPermissions());
            }
        }

        if (feature.getCustomProperties() != null && !CollectionUtils.isNullOrEmpty(feature.getCustomProperties().entrySet())) {
            Map<String, String> mapOfProperties = new HashMap<String, String>();
            for (Map.Entry<String, Property<?>> customP : feature.getCustomProperties().entrySet()) {
                if (customP.getValue() != null) {
                    mapOfProperties.put(customP.getKey(), customP.getValue().toJson());
                }
            }
            item.withMap(FEATURE_PROPERTIES, mapOfProperties);
        }
        return item;
    }

    @Override
    public Feature fromStore(Item item) {
        Feature feature = new Feature(item.getString(FEATURE_UID));
        feature.setEnable(item.getBoolean(FEATURE_ENABLE));
        feature.setDescription(item.getString(FEATURE_DESCRIPTION));
        feature.setGroup(item.getString(FEATURE_GROUP));

        String jsonFlippingStrategy = item.getJSON(FEATURE_STRATEGY);
        if (Util.hasLength(jsonFlippingStrategy)) {
            feature.setFlippingStrategy(FeatureJsonParser.parseFlipStrategyAsJson(feature.getUid(), jsonFlippingStrategy));
        }

        Set<String> perms = item.getStringSet(FEATURE_ROLE);
        feature.setPermissions(perms == null ? new HashSet<String>() : perms);

        Map<String, String> props = item.getMap(FEATURE_PROPERTIES);
        if (props != null) {
            Map<String, Property<?>> customProperties = new HashMap<String, Property<?>>();
            for (Map.Entry<String, String> propString : props.entrySet()) {
                customProperties.put(propString.getKey(), PropertyJsonParser.parseProperty(propString.getValue()));
            }
            feature.setCustomProperties(customProperties);
        }
        return feature;
    }
}
