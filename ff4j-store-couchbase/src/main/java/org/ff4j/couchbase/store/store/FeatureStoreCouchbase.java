package org.ff4j.couchbase.store.store;

/*
 * #%L
 * ff4j-store-springcouchbase
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import com.couchbase.client.java.Bucket;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.StringUtils;
import org.ff4j.core.Feature;
import org.ff4j.couchbase.store.document.FeatureDocument;
import org.ff4j.couchbase.store.mapper.DocumentMapper;
import org.ff4j.couchbase.store.mapper.ObjectMapperFactory;
import org.ff4j.couchbase.store.repository.CouchbaseRepository;
import org.ff4j.store.AbstractFeatureStore;
import org.ff4j.utils.Util;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Created by farrellyja on 09/11/2017.
 */
public class FeatureStoreCouchbase extends AbstractFeatureStore {

    /** Couchbase bucket connections **/
    private CouchbaseRepository<FeatureDocument> featureRepository;

    public FeatureStoreCouchbase(Bucket featureBucket) {
        ObjectMapper objectMapper = ObjectMapperFactory.createMapper();
        this.featureRepository = new CouchbaseRepository<>(featureBucket, FeatureDocument.class, objectMapper);
    }

    /** {@inheritDoc} */
    @Override
    public void createSchema() {

    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String uid) {
        Util.assertHasLength(uid);
        return featureRepository.get(uid) != null;
    }

    /** {@inheritDoc} */
    @Override
    public void enable(String uid) {
        assertFeatureExist(uid);
        FeatureDocument feature = featureRepository.get(uid);
        feature.setEnable(true);
        featureRepository.upsert(feature.getUid(), feature);
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String uid) {
        assertFeatureExist(uid);
        FeatureDocument feature = featureRepository.get(uid);
        feature.setEnable(false);
        featureRepository.upsert(feature.getUid(), feature);
    }

    /** {@inheritDoc} */
    @Override
    public void create(Feature fp) {
        assertFeatureNotNull(fp);
        assertFeatureNotExist(fp.getUid());
        featureRepository.upsert(fp.getUid(), DocumentMapper.featureToFeatureDocument(fp));
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String uid) {
        assertFeatureExist(uid);
        featureRepository.remove(uid);
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String uid) {
        assertFeatureExist(uid);
        FeatureDocument featureDocument = featureRepository.get(uid);
        return DocumentMapper.featureDocumentToFeature(featureDocument);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        List<FeatureDocument> featureDocuments = featureRepository.getAll();
        return featureDocuments.stream()
                .map(DocumentMapper::featureDocumentToFeature)
                .collect(Collectors.toMap(Feature::getUid, Function.identity()));
    }

    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        assertFeatureNotNull(fp);
        assertFeatureExist(fp.getUid());
        FeatureDocument featureDocument = DocumentMapper.featureToFeatureDocument(fp);
        featureRepository.upsert(featureDocument.getUid(), featureDocument);
    }

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String uid, String roleName) {
        Util.assertHasLength(roleName);
        assertFeatureExist(uid);
        FeatureDocument featureDocument = featureRepository.get(uid);
        featureDocument.getPermissions().add(roleName);
        featureRepository.upsert(featureDocument.getUid(), featureDocument);
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String uid, String roleName) {
        Util.assertHasLength(roleName);
        assertFeatureExist(uid);
        FeatureDocument featureDocument = featureRepository.get(uid);
        featureDocument.getPermissions().remove(roleName);
        featureRepository.upsert(featureDocument.getUid(), featureDocument);
    }

    /** {@inheritDoc} */
    @Override
    public void enableGroup(String groupName) {
        Map<String, Feature> featuresInGroup = readGroup(groupName);
        featuresInGroup.entrySet().forEach(kv -> {
            Feature f = kv.getValue();
            f.setGroup(groupName);
            f.setEnable(true);
            featureRepository.upsert(f.getUid(), DocumentMapper.featureToFeatureDocument(f));
        });
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
        assertGroupExist(groupName);
        Map<String, Feature> featuresInGroup = readGroup(groupName);
        featuresInGroup.entrySet().forEach(kv -> {
            Feature f = kv.getValue();
            f.setEnable(false);
            featureRepository.upsert(f.getUid(), DocumentMapper.featureToFeatureDocument(f));
        });
    }

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        Util.assertHasLength(groupName);
        return readAll().entrySet().stream()
            .filter(f -> groupName.equals(f.getValue().getGroup()))
            .count() > 0;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        assertGroupExist(groupName);
        return readAll().entrySet().stream()
                .filter(f -> groupName.equals(f.getValue().getGroup()))
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    }

    /** {@inheritDoc} */
    @Override
    public void addToGroup(String uid, String groupName) {
        Util.assertHasLength(groupName);
        assertFeatureExist(uid);
        FeatureDocument featureDocument = featureRepository.get(uid);
        featureDocument.setGroup(groupName);
        featureRepository.upsert(featureDocument.getUid(), featureDocument);
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String uid, String groupName) {
        Util.assertHasLength(groupName);
        assertFeatureExist(uid);
        assertGroupExist(groupName);
        FeatureDocument featureDocument = featureRepository.get(uid);
        featureDocument.setGroup(null);
        featureRepository.upsert(featureDocument.getUid(), featureDocument);
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> readAllGroups() {
        return readAll().entrySet().stream()
            .map(f -> f.getValue().getGroup())
            .filter(StringUtils::isNoneEmpty)
            .collect(Collectors.toSet());
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        featureRepository.removeAll();
    }
}
