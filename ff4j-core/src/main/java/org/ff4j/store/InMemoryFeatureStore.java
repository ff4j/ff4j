package org.ff4j.store;

/*
 * #%L ff4j-core $Id:$ $HeadURL:$ %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import java.io.InputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.ff4j.conf.XmlParser;
import org.ff4j.core.Feature;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;

/**
 * Storing states of feature inmemory with initial values. Could be used mostly for testing purpose.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class InMemoryFeatureStore extends AbstractFeatureStore {

    /** XML File where features are load. */
    private String fileName = null;

    /** InMemory Feature Map */
    private Map<String, Feature> featuresMap = new LinkedHashMap<String, Feature>();

    /** Group structure for features. */
    private Map<String, Set<String>> featureGroups = new HashMap<String, Set<String>>();

    /** Default constructor. */
    public InMemoryFeatureStore() {}

    /**
     * Constructor with configuration fileName.
     * 
     * @param fileName
     *            fileName present in classPath or on fileSystem.
     */
    public InMemoryFeatureStore(String fileName) {
        if (fileName == null || fileName.isEmpty()) {
            throw new IllegalArgumentException(
                    "fileName is required, cannot be null nor empty : the file must exist in classpath");
        }
        loadConfFile(fileName);
    }

    /**
     * Constructor with inputstream fileName.
     * 
     * @param fileName
     *            fileName present in classPath or on fileSystem.
     */
    public InMemoryFeatureStore(InputStream xmlIN) {
        loadConf(xmlIN);
    }

    /**
     * Constructor with full set of feature.
     * 
     * @param maps
     */
    public InMemoryFeatureStore(Map<String, Feature> maps) {
        this.featuresMap = maps;
        buildGroupsFromFeatures();
    }

    /**
     * Load configuration through FF4J.vml file.
     * 
     * @param conf
     *            xml filename
     */
    private void loadConfFile(String conf) {
        this.fileName = conf;
        loadConf(getClass().getClassLoader().getResourceAsStream(conf));
    }

    /**
     * Load configuration through FF4J.vml file.
     * 
     * @param conf
     *            xml filename
     */
    private void loadConf(InputStream xmlIN) {
        if (xmlIN == null) {
            throw new IllegalArgumentException("Cannot parse feature stream");
        }
        this.featuresMap = new XmlParser().parseConfigurationFile(xmlIN).getFeatures();
        buildGroupsFromFeatures();
    }

    /**
     * Group is an attribute of the feature and the group structure is rebuild from it.
     */
    private void buildGroupsFromFeatures() {
        // Reinit if required
        featureGroups = new HashMap<String, Set<String>>();
        for (Entry<String, Feature> item : featuresMap.entrySet()) {
            String currentGroup = item.getValue().getGroup();
            if (!featureGroups.containsKey(currentGroup)) {
                // Create Group of not exist
                featureGroups.put(currentGroup, new HashSet<String>());
            }
            featureGroups.get(currentGroup).add(item.getKey());
        }
    }

    /**
     * Unique update point to force group construction.
     * 
     * @param fp
     *            Target feature to update
     */
    private void updateFeature(Feature fp) {
        if (fp == null) {
            throw new IllegalArgumentException("Feature cannot be null nor empty");
        }
        featuresMap.put(fp.getUid(), fp);
        buildGroupsFromFeatures();
    }

    /** {@inheritDoc} */
    @Override
    public void create(Feature fp) {
        if (fp == null) {
            throw new IllegalArgumentException("Feature cannot be null nor empty");
        }
        if (exist(fp.getUid())) {
            throw new FeatureAlreadyExistException(fp.getUid());
        }
        updateFeature(fp);
    }

    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        if (fp == null) {
            throw new IllegalArgumentException("Feature cannot be null nor empty");
        }
        Feature fpExist = read(fp.getUid());
        // Checking new roles
        Set<String> toBeAdded = new HashSet<String>();
        toBeAdded.addAll(fp.getPermissions());
        toBeAdded.removeAll(fpExist.getPermissions());
        for (String addee : toBeAdded) {
            // Will fail if invalid userrole
            grantRoleOnFeature(fpExist.getUid(), addee);
        }
        updateFeature(fp);
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier (param#0) cannot be null nor empty");
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        featuresMap.remove(uid);
        buildGroupsFromFeatures();
    }

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String uid, String roleName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
        }
        if (roleName == null || roleName.isEmpty()) {
            throw new IllegalArgumentException("roleName cannot be null nor empty");
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        featuresMap.get(uid).getPermissions().add(roleName);
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String uid, String roleName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
        }
        if (roleName == null || roleName.isEmpty()) {
            throw new IllegalArgumentException("roleName cannot be null nor empty");
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        featuresMap.get(uid).getPermissions().remove(roleName);
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier (param#0) cannot be null nor empty");
        }
        return featuresMap.containsKey(uid);
    }

    /** {@inheritDoc} */
    @Override
    public void enable(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier (param#0) cannot be null nor empty");
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        featuresMap.get(uid).enable();
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        featuresMap.get(uid).disable();
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        return featuresMap.get(uid);
    }

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException("Groupname cannot be null nor empty");
        }
        return featureGroups.containsKey(groupName);
    }

    /** {@inheritDoc} */
    @Override
    public void enableGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException("Groupname cannot be null nor empty");
        }
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        for (String feat : featureGroups.get(groupName)) {
            this.enable(feat);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException("Groupname cannot be null nor empty");
        }
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        for (String feat : featureGroups.get(groupName)) {
            this.disable(feat);
        }
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException("groupName cannot be null nor empty");
        }
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        // Retrieve feature per feature (in-memory, no overhead)
        Map<String, Feature> features = new HashMap<String, Feature>();
        for (String feat : featureGroups.get(groupName)) {
            features.put(feat, this.read(feat));
        }
        return features;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> readAllGroups() {
        Set<String> groups = new HashSet<String>();
        groups.addAll(featureGroups.keySet());
        groups.remove(null);
        groups.remove("");
        return groups;
    }

    /** {@inheritDoc} */
    @Override
    public void addToGroup(String uid, String groupName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
        }
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException("Groupname cannot be null nor empty");
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        Feature feat = read(uid);
        feat.setGroup(groupName);
        update(feat);
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String uid, String groupName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
        }
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException("Groupname cannot be null nor empty");
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        Feature feat = read(uid);
        // --> Should not raise exception
        // if (feat.getGroup() != null && !feat.getGroup().equals(groupName)) {
        // throw new IllegalArgumentException("'" + uid + "' is not in group '" + groupName + "'");
        // }
        // <---
        feat.setGroup("");
        update(feat);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        return featuresMap;
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return toJson();
    }

    /** {@inheritDoc} */
    @Override
    public String toJson() {
        String json = super.toJson();
        // Remove last } to enrich the json document
        json = json.substring(0, json.length() - 1) + ",\"xmlInputFile\":";
        // No filename inputstream, set to true)
        if (null == fileName) {
            json+= "null";
        } else  {
            json += "\"" + this.fileName + "\"";
        }
        json+= "}";
        return json;
    }

    /**
     * Setter accessor for attribute 'locations'.
     * 
     * @param locations
     *            new value for 'locations '
     */
    public void setLocation(String locations) {
        loadConfFile(locations);
    }

    /**
     * Getter accessor for attribute 'fileName'.
     * 
     * @return current value of 'fileName'
     */
    public String getFileName() {
        return fileName;
    }

    // -------- Overrided in cache proxy --------------

    /** {@inheritDoc} */
    @Override
    public boolean isCached() {
        return false;
    }

    /** {@inheritDoc} */
    @Override
    public String getCacheProvider() {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public String getCachedTargetStore() {
        return null;
    }
}
