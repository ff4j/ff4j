package org.ff4j.arangodb;

import org.ff4j.arangodb.document.ArangoDBProperty;
import org.ff4j.property.Property;
import org.ff4j.property.util.PropertyFactory;

import java.util.Optional;
import java.util.Set;

import static java.util.stream.Collectors.toSet;

/*
 * #%L
 * ff4j-store-arangodb
 * %%
 * Copyright (C) 2013 - 2019 FF4J
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
}
