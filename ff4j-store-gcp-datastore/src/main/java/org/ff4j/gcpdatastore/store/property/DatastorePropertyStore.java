package org.ff4j.gcpdatastore.store.property;

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


import com.google.cloud.datastore.Datastore;
import com.google.cloud.datastore.DatastoreException;
import com.google.cloud.datastore.Entity;
import org.ff4j.exception.PropertyAccessException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.gcpdatastore.store.DatastoreClient;
import org.ff4j.gcpdatastore.store.EntityMapper;
import org.ff4j.gcpdatastore.store.StoreMapper;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.utils.Util;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.toMap;
import static java.util.stream.Collectors.toSet;

public class DatastorePropertyStore extends AbstractPropertyStore {
    private static final String DEFAULT_PROPERTY_STORE_KIND = "Ff4jProperty";

    private final DatastoreClient storeClient;

    /**
     * Constructor with datastore connection
     *
     * @param datastore the database connection
     */
    public DatastorePropertyStore(Datastore datastore) {
        storeClient = new DatastoreClient(datastore, DEFAULT_PROPERTY_STORE_KIND);
    }

    /**
     * Constructor with datastore connection and Namespace
     *
     * @param datastore the database connection
     * @param namespace the DB namespace in which the default Kind has to be created
     */
    public DatastorePropertyStore(Datastore datastore, String namespace) {
        storeClient = new DatastoreClient(datastore, namespace, DEFAULT_PROPERTY_STORE_KIND);
    }

    /**
     * Constructor with datastore connection, Namespace and Kind
     *
     * @param datastore the database connection
     * @param namespace the DB namespace in which the Kind has to be created
     * @param kind      the Kind to be created
     */
    public DatastorePropertyStore(Datastore datastore, String namespace, String kind) {
        storeClient = new DatastoreClient(datastore, namespace, kind);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean existProperty(String name) {
        Util.assertHasLength(name);
        return storeClient.keyExists(name);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <T> void createProperty(Property<T> value) {
        assertPropertyNotNull(value);
        assertPropertyNotExist(value.getName());

        DatastoreProperty datastoreProperty = StoreMapper.toPropertyStore(value);
        Entity entity = EntityMapper.toEntity(datastoreProperty, storeClient.getKeyFactory());
        try {
            storeClient.insert(entity);
        } catch (DatastoreException e) {
            throw new PropertyAccessException(datastoreProperty.getId(), e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Property<?> readProperty(String name) {
        assertPropertyExist(name);

        Optional<Entity> entity = storeClient.get(name);
        return entity
                .map(EntityMapper::fromPropertyEntity)
                .map(StoreMapper::fromPropertyStore)
                .orElseThrow(() -> new PropertyNotFoundException(name));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void deleteProperty(String name) {
        assertPropertyExist(name);

        try {
            storeClient.delete(name);
        } catch (DatastoreException e) {
            throw new PropertyAccessException(name, e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Property<?>> readAllProperties() {
        return getAllProperties().stream().collect(toMap(Property::getName, Function.identity()));
    }

    private List<? extends Property<?>> getAllProperties() {
        List<? extends Property<?>> properties = storeClient.getAll().stream()
                .map(EntityMapper::fromPropertyEntity)
                .map(StoreMapper::fromPropertyStore)
                .collect(Collectors.toList());
        return properties;
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
        storeClient.deleteAll();
    }
}
