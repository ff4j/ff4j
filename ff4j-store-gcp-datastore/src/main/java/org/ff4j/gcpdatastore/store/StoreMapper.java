package org.ff4j.gcpdatastore.store;

/*-
 * #%L
 * ff4j-store-gcp-datastore
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


import org.ff4j.audit.Event;
import org.ff4j.core.Feature;
import org.ff4j.gcpdatastore.store.event.DatastoreEvent;
import org.ff4j.gcpdatastore.store.feature.DatastoreFeature;
import org.ff4j.gcpdatastore.store.property.DatastoreProperty;
import org.ff4j.property.Property;
import org.ff4j.property.util.PropertyFactory;
import org.ff4j.utils.json.EventJsonParser;

import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;

import static java.util.stream.Collectors.toMap;
import static java.util.stream.Collectors.toSet;

public class StoreMapper {

    public static DatastoreFeature toFeatureStore(Feature feature) {
        Map<String, DatastoreProperty> customProperties = convertMap(feature.getCustomProperties(), StoreMapper::toPropertyStore);

        return DatastoreFeature.builder()
                .uid(feature.getUid())
                .enable(feature.isEnable())
                .description(feature.getDescription())
                .group(feature.getGroup())
                .permissions(feature.getPermissions())
                .flippingStrategy(feature.getFlippingStrategy())
                .customProperties(customProperties)
                .build();
    }

    public static Feature fromFeatureStore(DatastoreFeature datastoreFeature) {
        Map<String, Property<?>> customProperties = convertMap(datastoreFeature.getCustomProperties(), StoreMapper::fromPropertyStore);

        Feature feature = new Feature(datastoreFeature.getUid());
        feature.setEnable(datastoreFeature.isEnable());
        feature.setDescription(datastoreFeature.getDescription());
        feature.setGroup(datastoreFeature.getGroup());
        feature.setPermissions(datastoreFeature.getPermissions());
        feature.setFlippingStrategy(datastoreFeature.getFlippingStrategy());
        feature.setCustomProperties(customProperties);

        return feature;
    }

    public static DatastoreProperty toPropertyStore(Property<?> property) {
        Set<String> fixedValues = Optional.ofNullable(property.getFixedValues())
                .map(values -> values.stream().map(Object::toString).collect(toSet()))
                .orElse(null);

        return DatastoreProperty.builder()
                .id(property.getName())
                .readOnly(property.isReadOnly())
                .name(property.getName())
                .description(property.getDescription())
                .type(property.getType())
                .value(property.asString())
                .fixedValues(fixedValues)
                .build();
    }

    public static Property<?> fromPropertyStore(DatastoreProperty datastoreProperty) {
        return PropertyFactory.createProperty(
                datastoreProperty.getName(),
                datastoreProperty.getType(),
                datastoreProperty.getValue(),
                datastoreProperty.getDescription(),
                datastoreProperty.getFixedValues()
        );
    }

    public static DatastoreEvent toEventStore(Event e) {
        return DatastoreEvent.builder()
                .id(e.getUuid())
                .eventJson(e.toJson())
                .build();
    }

    public static Event fromEventStore(DatastoreEvent e) {
        return EventJsonParser.parseEvent(e.getEventJson());
    }

    private static <K, V, M> Map<K, M> convertMap(Map<K, V> map, Function<V, M> mapper) {
        return map.entrySet().stream().collect(toMap(Map.Entry::getKey, e -> mapper.apply(e.getValue())));
    }
}
