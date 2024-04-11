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
import org.ff4j.arangodb.document.ArangoDBFeature;
import org.ff4j.core.Feature;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.store.AbstractFeatureStore;
import org.ff4j.utils.Util;

import java.util.*;
import java.util.function.Function;
import java.util.function.Predicate;

import static java.util.stream.Collectors.*;

/**
 * Implementation of feature store for ArangoDB.
 *
 * @author nro-dw
 */
@Slf4j
public class FeatureStoreArangoDB extends AbstractFeatureStore {

    private static final String GET_ALL_FEATURES_ERROR = "Error while trying to get all features";
    private static final String CLEAR_FEATURES_ERROR = "Error while trying to clear all features";
    private static final String FIND_FEATURE_ERROR = "Error while finding feature";

    private final GenericArangoDBClient<ArangoDBFeature> featureClient;

    /**
     * @param featureCollection ArangoDB collection for features
     */
    public FeatureStoreArangoDB(final ArangoCollection featureCollection) {
        this.featureClient = new GenericArangoDBClient<>(featureCollection, ArangoDBFeature.class);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createSchema() {
        featureClient.initSchema();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean exist(final String featId) {
        Util.assertHasLength(featId);
        return featureClient.exists(featId);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void create(final Feature fp) {
        assertFeatureNotNull(fp);
        assertFeatureNotExist(fp.getUid());

        ArangoDBFeature arangoDbFeature = StoreMapper.toFeatureStore(fp);
        insertFeature(arangoDbFeature);
    }

    private void insertFeature(final ArangoDBFeature feature) {
        try {
            featureClient.insertDocument(feature);
        } catch (ArangoDBException e) {
            throw new FeatureAccessException(feature.getUid(), e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void delete(final String fpId) {
        assertFeatureExist(fpId);
        deleteFeature(fpId);
    }

    private void deleteFeature(final String uid) {
        try {
            featureClient.deleteDocument(uid);
        } catch (ArangoDBException e) {
            throw new FeatureAccessException(uid, e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void update(final Feature fp) {
        assertFeatureNotNull(fp);
        assertFeatureExist(fp.getUid());
        updateFeature(fp);
    }

    private void updateFeature(Feature fp) {
        try {
            ArangoDBFeature arangoDBFeature = StoreMapper.toFeatureStore(fp);
            featureClient.replaceDocument(fp.getUid(), arangoDBFeature);
        } catch (ArangoDBException e) {
            throw new FeatureAccessException(fp.getUid(), e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Feature read(final String featureUid) {
        assertFeatureExist(featureUid);
        return findFeature(featureUid).orElseThrow(() -> new FeatureNotFoundException(featureUid));
    }

    private Optional<Feature> findFeature(final String uid) {
        try {
            Optional<ArangoDBFeature> arangoDBFeature = Optional.ofNullable(featureClient.getDocument(uid));
            return arangoDBFeature.map(StoreMapper::fromFeatureStore);
        } catch (ArangoDBException e) {
            log.error(FIND_FEATURE_ERROR, e);
            return Optional.empty();
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Feature> readAll() {
        return getAllFeatures().stream().collect(toMap(Feature::getUid, Function.identity()));
    }

    private List<Feature> getAllFeatures() {
        try {
            return featureClient.getAll().stream().map(StoreMapper::fromFeatureStore).collect(toList());
        } catch (ArangoDBException e) {
            throw new FeatureAccessException(GET_ALL_FEATURES_ERROR, e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void clear() {
        try {
            featureClient.truncate();
        } catch (ArangoDBException e) {
            throw new FeatureAccessException(CLEAR_FEATURES_ERROR, e);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void enable(String uid) {
        this.updateStatus(uid, true);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void disable(String uid) {
        this.updateStatus(uid, false);
    }

    private void updateStatus(final String uid, final boolean enable) {
        assertFeatureExist(uid);
        findFeature(uid).ifPresent(feature -> {
            feature.setEnable(enable);
            updateFeature(feature);
        });
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean existGroup(String groupName) {
        Util.assertParamHasLength(groupName, "groupName");
        return !getGroupFeatures(groupName).isEmpty();
    }

    private List<Feature> getGroupFeatures(String group) {
        Predicate<Feature> groupNonNull = f -> Objects.nonNull(f.getGroup());
        Predicate<Feature> groupMatch = f -> group.equals(f.getGroup());

        try {
            return getAllFeatures().stream()
                    .filter(groupNonNull.and(groupMatch))
                    .collect(toList());
        } catch (ArangoDBException e) {
            throw new GroupNotFoundException(group);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        assertGroupExist(groupName);
        return getGroupFeatures(groupName).stream().collect(toMap(Feature::getUid, Function.identity()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void addToGroup(String featureId, String groupName) {
        Util.assertParamHasLength(groupName, "groupName");
        assertFeatureExist(featureId);

        findFeature(featureId).ifPresent(feature -> {
            feature.setGroup(groupName);
            updateFeature(feature);
        });
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void removeFromGroup(String featureId, String groupName) {
        assertFeatureExist(featureId);
        assertGroupExist(groupName);

        findFeature(featureId).ifPresent(feature -> {
            feature.setGroup("");
            updateFeature(feature);
        });
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void enableGroup(String groupName) {
        assertGroupExist(groupName);
        updateGroupStatus(groupName, true);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void disableGroup(String groupName) {
        assertGroupExist(groupName);
        updateGroupStatus(groupName, false);
    }

    private void updateGroupStatus(final String groupName, final boolean enable) {
        getGroupFeatures(groupName).forEach(feature -> {
            feature.setEnable(enable);
            updateFeature(feature);
        });
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<String> readAllGroups() {
        Predicate<String> stringNotEmpty = s -> !s.isEmpty();

        return getAllFeatures().stream()
                .map(Feature::getGroup)
                .filter(Objects::nonNull)
                .filter(stringNotEmpty)
                .collect(toSet());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void grantRoleOnFeature(String flipId, String roleName) {
        Util.assertParamHasLength(roleName, "roleName");
        assertFeatureExist(flipId);

        findFeature(flipId).ifPresent(feature -> {
            feature.getPermissions().add(roleName);
            updateFeature(feature);
        });
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void removeRoleFromFeature(String flipId, String roleName) {
        Util.assertParamHasLength(roleName, "roleName");
        assertFeatureExist(flipId);

        findFeature(flipId).ifPresent(feature -> {
            feature.getPermissions().remove(roleName);
            updateFeature(feature);
        });
    }
}
