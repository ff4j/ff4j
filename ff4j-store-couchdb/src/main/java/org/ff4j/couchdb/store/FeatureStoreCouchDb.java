package org.ff4j.couchdb.store;

/*-
 * #%L
 * ff4j-store-couchdb
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

import static org.ff4j.couchdb.CouchDbConstants.DEFAULT_FEATURE_TYPE;

import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import org.ektorp.CouchDbConnector;
import org.ektorp.DocumentNotFoundException;
import org.ektorp.UpdateConflictException;
import org.ff4j.core.Feature;
import org.ff4j.couchdb.CouchDbConnection;
import org.ff4j.couchdb.CouchDbFeatureView;
import org.ff4j.couchdb.document.CouchDbFeature;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.store.AbstractFeatureStore;
import org.ff4j.utils.Util;
import org.ff4j.utils.json.FeatureJsonParser;
import org.lightcouch.CouchDbException;

/**
 * Implementation of store using {@link CouchDbConnection} connection.
 *
 * @author Curtis White (@drizztguen77)
 */
public class FeatureStoreCouchDb extends AbstractFeatureStore {

    /**
     * error message.
     */
    public static final String FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY = "Feature identifier cannot be null nor empty";

    /**
     * error message.
     */
    public static final String GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY = "Groupname cannot be null nor empty";

    /**
     * Current CouchDB connection.
     */
    private CouchDbConnection couchDbConnection;

    /**
     * Current CouchDB connector.
     */
    private CouchDbConnector couchDbConnector;

    /**
     * Repository class to query couchDB
     */
    private CouchDbFeatureView couchDbFeatureView;
   
    /**
     * Default constructor
     */
    public FeatureStoreCouchDb() {
    }

    /**
     * Parameterized constructor with database connection.
     *
     * @param couchDbConnection the database connection to set
     * @param couchDbFeatureView feature repository
     */
    public FeatureStoreCouchDb(CouchDbConnection couchDbConnection, CouchDbFeatureView couchDbFeatureView) {
        this.couchDbConnection = couchDbConnection;
        this.couchDbConnector = couchDbConnection.getCouchDbConnector();
        this.couchDbFeatureView = couchDbFeatureView;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void enable(String featId) {
        this.updateStatus(featId, true);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void disable(String featId) {
        this.updateStatus(featId, false);
    }

    /**
     * Update status of feature.
     *
     * @param uid    feature id
     * @param enable enabler
     */
    private void updateStatus(String uid, boolean enable) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }

        CouchDbFeature couchDbFeature = couchDbConnector.find(CouchDbFeature.class, uid);
        Feature feature = fromJson(couchDbFeature.getFeature());
        if (null != feature) {
            feature.setEnable(enable);
            couchDbFeature.setFeature(feature.toJson());
            updateFeature(couchDbFeature);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean exist(String featId) {
        Util.assertHasLength(featId);

        try {
            getFeature(featId);
        } catch (FeatureNotFoundException fne) {
            return false;
        }

        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Feature read(String uid) {
        assertFeatureExist(uid);
        Feature feat = fromJson(getFeature(uid).getFeature());
        return feat;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void create(Feature fp) {
        if (fp == null) {
            throw new IllegalArgumentException("Feature cannot be null nor empty");
        }
        if (exist(fp.getUid())) {
            throw new FeatureAlreadyExistException(fp.getUid());
        }

        CouchDbFeature couchDbFeature = new CouchDbFeature();
        couchDbFeature.setType(DEFAULT_FEATURE_TYPE);
        couchDbFeature.setFeature(fp.toJson());
        couchDbFeature.setId(fp.getUid());
        createFeature(couchDbFeature);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void delete(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }

        CouchDbFeature couchDbFeature = getFeature(uid);
        removeFeature(couchDbFeature);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void grantRoleOnFeature(String uid, String roleName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (roleName == null || roleName.isEmpty()) {
            throw new IllegalArgumentException("roleName cannot be null nor empty");
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }

        CouchDbFeature couchDbFeature = getFeature(uid);
        Feature feature = fromJson(couchDbFeature.getFeature());
        if (null != feature) {
            feature.getPermissions().add(roleName);
            couchDbFeature.setFeature(feature.toJson());
            updateFeature(couchDbFeature);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void removeRoleFromFeature(String uid, String roleName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (roleName == null || roleName.isEmpty()) {
            throw new IllegalArgumentException("roleName cannot be null nor empty");
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }

        CouchDbFeature couchDbFeature = getFeature(uid);
        Feature feature = fromJson(couchDbFeature.getFeature());
        if (null != feature) {
            feature.getPermissions().remove(roleName);
            couchDbFeature.setFeature(feature.toJson());
            updateFeature(couchDbFeature);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Feature> readAll() {
        LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<>();

        List<CouchDbFeature> features = getFeatures();
        features.forEach(f -> mapFP.put(f.getId(), fromJson(f.getFeature())));

        return mapFP;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void update(Feature fp) {
        if (fp == null) {
            throw new IllegalArgumentException("Feature cannot be null nor empty");
        }

        CouchDbFeature feature = getFeature(fp.getUid());
        feature.setFeature(fp.toJson());
        updateFeature(feature);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean existGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }

        List<CouchDbFeature> features = getFeatures(groupName);

        return null != features && features.size() > 0;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<String> readAllGroups() {
        Set<String> setOfGroups = new HashSet<>();

        List<CouchDbFeature> features = getFeatures();

        features.forEach(f -> {
            Feature feat = fromJson(f.getFeature());
            if (null != feat) {
                setOfGroups.add(feat.getGroup());
            }
        });
        setOfGroups.remove(null);
        setOfGroups.remove("");
        return setOfGroups;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }

        LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<>();

        List<CouchDbFeature> features = getFeatures(groupName);
        if (null != features) {
            features.forEach(f -> {
                Feature feat = fromJson(f.getFeature());
                if (null != feat) {
                    mapFP.put(f.getId(), feat);
                }
            });
        }

        return mapFP;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void enableGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }

        List<CouchDbFeature> features = getFeatures(groupName);
        if (null != features) {
            features.forEach(f -> {
                Feature feat = fromJson(f.getFeature());
                if (null != feat) {
                    feat.setEnable(true);
                    f.setFeature(feat.toJson());
                    updateFeature(f);
                }
            });
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void disableGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }

        List<CouchDbFeature> features = getFeatures(groupName);
        if (null != features) {
            features.forEach(f -> {
                Feature feat = fromJson(f.getFeature());
                if (null != feat) {
                    feat.setEnable(false);
                    f.setFeature(feat.toJson());
                    updateFeature(f);
                }
            });
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void addToGroup(String uid, String groupName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }

        CouchDbFeature couchDbFeature = getFeature(uid);
        Feature feat = fromJson(couchDbFeature.getFeature());
        if (null != feat) {
            feat.setGroup(groupName);
            couchDbFeature.setFeature(feat.toJson());
            updateFeature(couchDbFeature);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void removeFromGroup(String uid, String groupName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }

        CouchDbFeature couchDbFeature = getFeature(uid);
        Feature feat = fromJson(couchDbFeature.getFeature());
        if (null != feat) {
            feat.setGroup("");
            couchDbFeature.setFeature(feat.toJson());
            updateFeature(couchDbFeature);
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void clear() {
        // Delete all the features
        List<CouchDbFeature> features = getFeatures();
        features.forEach(f -> this.couchDbConnector.delete(f));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createSchema() {
        // Do nothing here since CouchDB does not have collection or any other table
        // structure. There is nothing to create
    }

    /**
     * Gets a list of all features in the database
     *
     * @return list of CouchDB features
     */
    public List<CouchDbFeature> getFeatures() {

        try {
            return couchDbFeatureView.getAll();
            //return this.couchDbConnector.findDocs("{\"selector\": {\"type\": \"" + DEFAULT_FEATURE_TYPE + "\"}}", CouchDbFeature.class);
        } catch (CouchDbException cde) {
            return null;
        }
    }

    /**
     * Gets a list of all features in a specified group from the database
     *
     * @param group group to get features for
     * @return list of CouchDB features
     * @throws GroupNotFoundException If no documents were found for the specified group.
     */
    public List<CouchDbFeature> getFeatures(String group) {

        try {
            return couchDbFeatureView.getAll().stream().filter(f -> {
                Feature feat = fromJson(f.getFeature());
                return null != feat && null != feat.getGroup() && feat.getGroup().equals(group);
            }).collect(Collectors.toList());
            //return this.couchDbConnector.findDocs("{\"selector\": {\"type\": \"" + DEFAULT_FEATURE_TYPE + "\",\"feature.groupName\": \"" + group + "\"}}", CouchDbFeature.class);
        } catch (CouchDbException cde) {
            throw new GroupNotFoundException(group);
        }
    }

    /**
     * Get a single feature from CouchDB
     *
     * @param uid UUID of the feature to get
     * @return the CouchDB feature
     * @throws FeatureNotFoundException If the document is not found in the database.
     */
    public CouchDbFeature getFeature(String uid) {
        try {
            CouchDbFeature couchDbFeature = this.couchDbFeatureView.get(uid);
            return couchDbFeature;
//            return this.couchDbConnector.get(CouchDbFeature.class, uid);

        } catch (DocumentNotFoundException dnf) {
            throw new FeatureNotFoundException(uid);
        }
    }

    /**
     * Create a single CouchDB feature.
     *
     * @param couchDbFeature CouchDB feature to create
     * @throws FeatureAlreadyExistException If a conflict is detected during the create.
     */
    public void createFeature(CouchDbFeature couchDbFeature) {

        try {
            this.couchDbFeatureView.add(couchDbFeature);
//            this.couchDbConnector.create(couchDbFeature);
        } catch (UpdateConflictException uce) {
            throw new FeatureAlreadyExistException(couchDbFeature.getFeature());
        }
    }

    /**
     * Update a single CouchDB feature. If the document does not exist it will be created.
     *
     * @param couchDbFeature CouchDB feature to update
     * @throws FeatureAlreadyExistException If a conflict is detected during the update.
     */
    public void updateFeature(CouchDbFeature couchDbFeature) {

        try {
            this.couchDbFeatureView.update(couchDbFeature);
//            this.couchDbConnector.update(couchDbFeature);
        } catch (UpdateConflictException uce) {
            throw new FeatureAlreadyExistException(couchDbFeature.getFeature());
        }
    }

    /**
     * Update a single CouchDB feature
     *
     * @param couchDbFeature CouchDB feature to update
     * @throws FeatureNotFoundException If the document is not found in the database.
     */
    public void removeFeature(CouchDbFeature couchDbFeature) {

        try {
            this.couchDbFeatureView.remove(couchDbFeature);
//            this.couchDbConnector.delete(couchDbFeature);
        } catch (DocumentNotFoundException | UpdateConflictException e) {
            throw new FeatureNotFoundException(couchDbFeature.getId());
        }
    }

    private Feature fromJson(String json) {
        return FeatureJsonParser.parseFeature(json);
    }

    /**
     * Setter to set the CouchDB connection
     *
     * @param couchDbConnection CouchDB connection
     */
    public void setCouchDbConnector(CouchDbConnection couchDbConnection) {
        this.couchDbConnection = couchDbConnection;
    }

    /**
     * Setter to set the feature repository
     *
     * @param couchDbFeatureView Feature repository
     */
    public void setCouchDbFeatureView(CouchDbFeatureView couchDbFeatureView) {
        this.couchDbFeatureView = couchDbFeatureView;
    }

    /**
     * Getter accessor for attribute 'couchDbConnection'.
     *
     * @return
     *       current value of 'couchDbConnection'
     */
    public CouchDbConnection getCouchDbConnection() {
        return couchDbConnection;
    }
}
