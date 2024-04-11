package org.ff4j.arangodb.store;

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

import com.arangodb.ArangoCollection;
import com.arangodb.ArangoDBException;
import lombok.extern.slf4j.Slf4j;
import org.ff4j.arangodb.StoreMapper;
import org.ff4j.arangodb.document.ArangoDBProperty;
import org.ff4j.exception.PropertyAccessException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.utils.Util;

import java.util.*;
import java.util.function.Function;

import static java.util.stream.Collectors.*;

/**
 * Implementation of property store for ArangoDB.
 *
 * @author nro-dw
 */
@Slf4j
public class PropertyStoreArangoDB extends AbstractPropertyStore {

    private static final String GET_ALL_PROPERTIES_ERROR = "Error while trying to get all properties";
    private static final String CLEAR_PROPERTIES_ERROR = "Error while trying to delete all properties";
    private static final String FIND_PROPERTY_ERROR = "Error while finding property";

    private final GenericArangoDBClient<ArangoDBProperty> propertyClient;

    /**
     * @param propertyCollection ArangoDB collection for properties
     */
    public PropertyStoreArangoDB(final ArangoCollection propertyCollection) {
        this.propertyClient = new GenericArangoDBClient<>(propertyCollection, ArangoDBProperty.class);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createSchema() {
        propertyClient.initSchema();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean existProperty(final String name) {
        Util.assertHasLength(name);
        return propertyClient.exists(name);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <T> void createProperty(final Property<T> prop) {
        assertPropertyNotNull(prop);
        assertPropertyNotExist(prop.getName());

        ArangoDBProperty arangoDBProperty = StoreMapper.toPropertyStore(prop);
        insertProperty(arangoDBProperty);
    }

    private void insertProperty(ArangoDBProperty property) {
        try {
            propertyClient.insertDocument(property);
        } catch (ArangoDBException e) {
            throw new PropertyAccessException(property.getName(), e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Property<?> readProperty(final String name) {
        assertPropertyExist(name);
        return findProperty(name).orElseThrow(() -> new PropertyNotFoundException(name));
    }

    private Optional<? extends Property<?>> findProperty(final String name) {
        try {
            Optional<ArangoDBProperty> arangoDBProperty = Optional.ofNullable(propertyClient.getDocument(name));
            return arangoDBProperty.map(StoreMapper::fromPropertyStore).filter(Objects::nonNull);
        } catch (ArangoDBException e) {
            log.error(FIND_PROPERTY_ERROR, e);
            return Optional.empty();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void deleteProperty(final String name) {
        assertPropertyExist(name);
        deleteArangoProperty(name);
    }

    private void deleteArangoProperty(final String name) {
        try {
            propertyClient.deleteDocument(name);
        } catch (ArangoDBException e) {
            throw new PropertyAccessException(name, e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <T> void updateProperty(final Property<T> prop) {
        assertPropertyNotNull(prop);
        assertPropertyExist(prop.getName());

        ArangoDBProperty arangoDBProperty = StoreMapper.toPropertyStore(prop);
        replaceProperty(arangoDBProperty);
    }

    private void replaceProperty(ArangoDBProperty property) {
        try {
            propertyClient.replaceDocument(property.getName(), property);
        } catch (ArangoDBException e) {
            throw new PropertyAccessException(property.getName(), e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Property<?>> readAllProperties() {
        return getAllProperties().stream().collect(toMap(Property::getName, Function.identity()));
    }

    private List<Property<?>> getAllProperties() {
        try {
            return propertyClient.getAll().stream()
                    .map(StoreMapper::fromPropertyStore)
                    .filter(Objects::nonNull)
                    .collect(toList());
        } catch (ArangoDBException e) {
            throw new PropertyAccessException(GET_ALL_PROPERTIES_ERROR, e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<String> listPropertyNames() {
        return getAllProperties().stream().map(Property::getName).collect(toSet());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void clear() {
        try {
            propertyClient.truncate();
        } catch (ArangoDBException e) {
            throw new PropertyAccessException(CLEAR_PROPERTIES_ERROR, e);
        }
    }
}
