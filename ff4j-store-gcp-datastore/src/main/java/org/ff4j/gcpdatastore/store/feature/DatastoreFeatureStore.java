package org.ff4j.gcpdatastore.store.feature;

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

import com.google.cloud.datastore.*;
import org.ff4j.core.Feature;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.gcpdatastore.store.DatastoreClient;
import org.ff4j.gcpdatastore.store.EntityMapper;
import org.ff4j.gcpdatastore.store.StoreMapper;
import org.ff4j.store.AbstractFeatureStore;
import org.ff4j.utils.Util;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.toMap;

public class DatastoreFeatureStore extends AbstractFeatureStore {
    private static final String DEFAULT_FEATURE_STORE_KIND = "Ff4jFeature";

    private final DatastoreClient storeClient;

    /**
     * Constructor with datastore connection
     *
     * @param datastore the database connection
     */
    public DatastoreFeatureStore(Datastore datastore) {
        storeClient = new DatastoreClient(datastore, DEFAULT_FEATURE_STORE_KIND);
    }

    /**
     * Constructor with datastore connection and Namespace
     *
     * @param datastore the database connection
     * @param namespace the DB namespace in which the default Kind has to be created
     */
    public DatastoreFeatureStore(Datastore datastore, String namespace) {
        storeClient = new DatastoreClient(datastore, namespace, DEFAULT_FEATURE_STORE_KIND);
    }

    /**
     * Constructor with datastore connection, Namespace and Kind
     *
     * @param datastore the database connection
     * @param namespace the DB namespace in which the Kind has to be created
     * @param kind the Kind to be created
     */
    public DatastoreFeatureStore(Datastore datastore, String namespace, String kind) {
        storeClient = new DatastoreClient(datastore, namespace, kind);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean exist(String featId) {
        Util.assertHasLength(featId);
        return storeClient.keyExists(featId);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void create(Feature fp) {
        assertFeatureNotNull(fp);
        assertFeatureNotExist(fp.getUid());

        DatastoreFeature gcpDatastoreFeature = StoreMapper.toFeatureStore(fp);
        Entity entity = EntityMapper.toEntity(gcpDatastoreFeature, storeClient.getKeyFactory());
        try {
            storeClient.insert(entity);
        } catch (DatastoreException e) {
            throw new FeatureAccessException(gcpDatastoreFeature.getUid(), e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Feature read(String featureUid) {
        assertFeatureExist(featureUid);

        Optional<Entity> entity = storeClient.get(featureUid);
        return entity
                .map(EntityMapper::fromEntity)
                .map(StoreMapper::fromFeatureStore)
                .orElseThrow(() -> new FeatureNotFoundException(featureUid));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Feature> readAll() {
        List<Feature> features = storeClient.getAll().stream()
                .map(EntityMapper::fromEntity)
                .map(StoreMapper::fromFeatureStore)
                .collect(Collectors.toList());

        return features.stream().collect(toMap(Feature::getUid, Function.identity()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void delete(String fpId) {
        assertFeatureExist(fpId);

        try {
            storeClient.delete(fpId);
        } catch (DatastoreException e) {
            throw new FeatureAccessException(fpId, e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void update(Feature fp) {
        assertFeatureNotNull(fp);
        assertFeatureExist(fp.getUid());

        DatastoreFeature gcpDatastoreFeature = StoreMapper.toFeatureStore(fp);
        FullEntity<Key> entity = EntityMapper.toEntity(gcpDatastoreFeature, storeClient.getKeyFactory());
        try {
            storeClient.update(entity);
        } catch (DatastoreException e) {
            throw new FeatureAccessException(gcpDatastoreFeature.getUid(), e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void clear() {
        storeClient.deleteAll();
    }
}
