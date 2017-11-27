package org.ff4j.couchbase.store;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.ff4j.core.Feature;
import org.ff4j.couchbase.CouchbaseConnection;
import org.ff4j.couchbase.mapper.FeatureCouchbaseMapper;
import org.ff4j.store.AbstractFeatureStore;
import org.ff4j.utils.Util;
import org.ff4j.utils.json.FeatureJsonParser;

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
import com.couchbase.client.java.query.N1qlQuery;
import com.couchbase.client.java.query.N1qlQueryResult;
import com.couchbase.client.java.query.N1qlQueryRow;

/**
 * Implementation of FeatureStore into Couchbase.
 *
 * @author farrellyja
 * @author Cedrick LUNVEN (@clunven)
 */
public class FeatureStoreCouchbase extends AbstractFeatureStore {
    
    /** Couchebase mapper. */
    private FeatureCouchbaseMapper FEATURE_MAPPER = new FeatureCouchbaseMapper();
    
    /** Keep reference to connection. */
    private CouchbaseConnection couchBaseConnection;
    
    /** Keep reference to bucket. */
    private Bucket featureBucket;
    
    /**
     * Default initialisation
     */
    public FeatureStoreCouchbase() {
    }
    
    /**
     * Initialization thourhg connection
     * @param conn
     */
    public FeatureStoreCouchbase(CouchbaseConnection conn) {
        this.couchBaseConnection = conn;
    }
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        throw new UnsupportedOperationException("Cannot create buckets from Java driver");
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String uid) {
        Util.assertHasLength(uid);
        return getFeatureBucket().exists(uid);
    }

    /** {@inheritDoc} */
    @Override
    public void enable(String uid) {
        assertFeatureExist(uid);
        Feature f1 = FEATURE_MAPPER.fromStore(getFeatureBucket().get(uid));
        f1.enable();
        update(f1);
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String uid) {
        assertFeatureExist(uid);
        Feature f1 = FEATURE_MAPPER.fromStore(getFeatureBucket().get(uid));
        f1.disable();
        update(f1);
    }

    /** {@inheritDoc} */
    @Override
    public void create(Feature fp) {
        assertFeatureNotNull(fp);
        assertFeatureNotExist(fp.getUid());
        getFeatureBucket().upsert(FEATURE_MAPPER.toStore(fp));
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String uid) {
        assertFeatureExist(uid);
        getFeatureBucket().remove(uid);
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String uid) {
        assertFeatureExist(uid);
        return FEATURE_MAPPER.fromStore(getFeatureBucket().get(uid));
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        // To retrieve the items we use the BUCKET (not view as KEY/VALUE searches)
        N1qlQuery queryFeatures = N1qlQuery.simple("SELECT * FROM " + couchBaseConnection.getFf4jFeatureBucketName());
        N1qlQueryResult queryResult = getFeatureBucket().query(queryFeatures);
        Map<String, Feature> allFeatures = new HashMap<>();
        for (N1qlQueryRow row : queryResult.allRows()) {
            Feature f = FeatureJsonParser.parseFeature(row.value().get(couchBaseConnection.getFf4jFeatureBucketName()).toString());
            allFeatures.put(f.getUid(), f);
        }
        return allFeatures;
    }
    
   
    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        assertFeatureNotNull(fp);
        assertFeatureExist(fp.getUid());
        getFeatureBucket().upsert(FEATURE_MAPPER.toStore(fp));
    }

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String uid, String roleName) {
        Util.assertHasLength(roleName);
        assertFeatureExist(uid);
        Feature f = read(uid);
        f.getPermissions().add(roleName);
        update(f);
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String uid, String roleName) {
        Util.assertHasLength(roleName);
        assertFeatureExist(uid);
        Feature f = read(uid);
        f.getPermissions().remove(roleName);
        update(f);
    }

    /** {@inheritDoc} */
    @Override
    public void enableGroup(String groupName) {
        Map<String, Feature> featuresInGroup = readGroup(groupName);
        featuresInGroup.entrySet().forEach(kv -> {
            Feature f = kv.getValue();
            f.setGroup(groupName);
            f.setEnable(true);
            update(f);
        });
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
        Map<String, Feature> featuresInGroup = readGroup(groupName);
        featuresInGroup.entrySet().forEach(kv -> {
            Feature f = kv.getValue();
            f.setGroup(groupName);
            f.setEnable(false);
            update(f);
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
        Feature f = read(uid);
        f.setGroup(groupName);
        update(f);
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String uid, String groupName) {
        Util.assertHasLength(groupName);
        assertFeatureExist(uid);
        Feature f = readGroup(groupName).get(uid);
        if (f != null) {
            f.setGroup(null);
            update(f);
        }
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> readAllGroups() {
        return readAll().entrySet().stream()
            .map(f -> f.getValue().getGroup())
            .filter(Util::hasLength).collect(Collectors.toSet());
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        getFeatureBucket().bucketManager().flush();
    }
    
    /**
     * Access to feature bucket.
     *
     * @return
     *      reference to bucket
     */
    private Bucket getFeatureBucket() {
        if (featureBucket == null) {
            Util.assertNotNull(getCouchBaseConnection());
            featureBucket = getCouchBaseConnection().getFeaturesBucket();
            Util.assertNotNull(featureBucket);
        }
        return featureBucket;
    }

    /**
     * Getter accessor for attribute 'couchBaseConnection'.
     *
     * @return
     *       current value of 'couchBaseConnection'
     */
    public CouchbaseConnection getCouchBaseConnection() {
        return couchBaseConnection;
    }

    /**
     * Setter accessor for attribute 'couchBaseConnection'.
     * @param couchBaseConnection
     * 		new value for 'couchBaseConnection '
     */
    public void setCouchBaseConnection(CouchbaseConnection couchBaseConnection) {
        this.couchBaseConnection = couchBaseConnection;
    }
}
