package org.ff4j.dynamodb.feature;

/*-
 * #%L
 * ff4j-store-aws-dynamodb
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


import org.ff4j.core.Feature;
import org.ff4j.mapper.FeatureMapper;
import org.ff4j.property.Property;
import org.ff4j.utils.JsonUtils;
import org.ff4j.utils.Util;
import org.ff4j.utils.json.FeatureJsonParser;
import org.ff4j.utils.json.PropertyJsonParser;
import software.amazon.awssdk.utils.CollectionUtils;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;


/**
 * Implementation of {@link FeatureMapper} for DynamoDB store
 *
 * @author <a href="mailto:jeromevdl@gmail.com">Jerome VAN DER LINDEN</a>
 */
public class FeatureDynamoDBMapper implements FeatureMapper<DynamoDbFeature> {

    @Override
    public DynamoDbFeature toStore(Feature feature) {
        DynamoDbFeature item = new DynamoDbFeature();
        item.setFeatureUid(feature.getUid());
        item.setEnable(feature.isEnable());
        item.setDescription(feature.getDescription());
        item.setGroupName(feature.getGroup());

        if (feature.getFlippingStrategy() != null) {
            item.setFlippingStrategy(JsonUtils.flippingStrategyAsJson(feature.getFlippingStrategy()));
        }

        if (!CollectionUtils.isNullOrEmpty(feature.getPermissions())) {
            item.setPermissions(feature.getPermissions());
        }

        if (feature.getCustomProperties() != null && !CollectionUtils.isNullOrEmpty(feature.getCustomProperties().entrySet())) {
            Map<String, String> mapOfProperties = new HashMap<>();
            for (Map.Entry<String, Property<?>> customP : feature.getCustomProperties().entrySet()) {
                if (customP.getValue() != null) {
                    mapOfProperties.put(customP.getKey(), customP.getValue().toJson());
                }
            }
            item.setProperties(mapOfProperties);
        }
        return item;
    }

    @Override
    public Feature fromStore(DynamoDbFeature item) {
        Feature feature = new Feature(item.getFeatureUid());
        feature.setEnable(item.isEnable());
        feature.setDescription(item.getDescription());
        feature.setGroup(item.getGroupName());

        String jsonFlippingStrategy = item.getFlippingStrategy();
        if (Util.hasLength(jsonFlippingStrategy)) {
            feature.setFlippingStrategy(FeatureJsonParser.parseFlipStrategyAsJson(feature.getUid(), jsonFlippingStrategy));
        }

        Set<String> perms = item.getPermissions();
        feature.setPermissions(perms == null ? new HashSet<>() : perms);

        Map<String, String> props = item.getProperties();
        if (props != null) {
            Map<String, Property<?>> customProperties = new HashMap<>();
            for (Map.Entry<String, String> propString : props.entrySet()) {
                customProperties.put(propString.getKey(), PropertyJsonParser.parseProperty(propString.getValue()));
            }
            feature.setCustomProperties(customProperties);
        }
        return feature;
    }
}
