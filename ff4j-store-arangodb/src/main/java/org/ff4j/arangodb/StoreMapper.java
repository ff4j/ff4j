package org.ff4j.arangodb;

/*-
 * #%L
 * ff4j-store-arangodb
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

import org.ff4j.arangodb.document.ArangoDBEvent;
import org.ff4j.arangodb.document.ArangoDBFeature;
import org.ff4j.arangodb.document.ArangoDBProperty;
import org.ff4j.audit.Event;
import org.ff4j.core.Feature;
import org.ff4j.property.Property;
import org.ff4j.property.util.PropertyFactory;
import org.ff4j.utils.json.EventJsonParser;

import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;

import static java.util.stream.Collectors.toMap;
import static java.util.stream.Collectors.toSet;

/**
 * Utility methods for mapping ArangoDB documents to FF4J objects
 */
public class StoreMapper {

    public static ArangoDBProperty toPropertyStore(Property<?> p) {
        Set<String> fixedValues = Optional.ofNullable(p.getFixedValues())
                .map(values -> values.stream().map(Object::toString).collect(toSet()))
                .orElse(null);

        return ArangoDBProperty.builder()
                .id(p.getName())
                .readOnly(p.isReadOnly())
                .name(p.getName())
                .description(p.getDescription())
                .type(p.getType())
                .value(p.asString())
                .fixedValues(fixedValues)
                .build();
    }

    public static Property<?> fromPropertyStore(ArangoDBProperty property) {
        return PropertyFactory.createProperty(
                property.getName(),
                property.getType(),
                property.getValue(),
                property.getDescription(),
                property.getFixedValues()
        );
    }

    public static ArangoDBFeature toFeatureStore(Feature f) {
        Map<String, ArangoDBProperty> customProperties = convertMap(f.getCustomProperties(), StoreMapper::toPropertyStore);

        return ArangoDBFeature.builder()
                .id(f.getUid())
                .uid(f.getUid())
                .enable(f.isEnable())
                .description(f.getDescription())
                .group(f.getGroup())
                .permissions(f.getPermissions())
                .flippingStrategy(f.getFlippingStrategy())
                .customProperties(customProperties)
                .build();
    }

    public static Feature fromFeatureStore(ArangoDBFeature f) {
        Map<String, Property<?>> customProperties = convertMap(f.getCustomProperties(), StoreMapper::fromPropertyStore);

        Feature feature = new Feature(f.getId());
        feature.setEnable(f.isEnable());
        feature.setDescription(f.getDescription());
        feature.setGroup(f.getGroup());
        feature.setPermissions(f.getPermissions());
        feature.setFlippingStrategy(f.getFlippingStrategy());
        feature.setCustomProperties(customProperties);

        return feature;
    }

    private static <K, V, M> Map<K, M> convertMap(Map<K, V> map, Function<V, M> mapper) {
        return map.entrySet().stream().collect(toMap(Map.Entry::getKey, e -> mapper.apply(e.getValue())));
    }

    public static ArangoDBEvent toEventStore(Event e) {
        return new ArangoDBEvent(e.getUuid(), e.toJson());
    }

    public static Event fromEventStore(ArangoDBEvent e) {
        return EventJsonParser.parseEvent(e.getAsJson());
    }
}
