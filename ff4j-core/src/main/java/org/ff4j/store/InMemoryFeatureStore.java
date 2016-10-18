package org.ff4j.store;

import static org.ff4j.utils.Util.assertHasLength;

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
        createSchema();
        loadConfFile(fileName);
    }

    /**
     * Constructor with inputstream fileName.
     * 
     * @param fileName
     *            fileName present in classPath or on fileSystem.
     */
    public InMemoryFeatureStore(InputStream xmlIN) {
        createSchema();
        loadConf(xmlIN);
    }

    /**
     * Constructor with full set of feature.
     * 
     * @param maps
     */
    public InMemoryFeatureStore(Map<String, Feature> maps) {
        createSchema();
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
        featuresMap.put(fp.getUid(), fp);
        buildGroupsFromFeatures();
    }

    /** {@inheritDoc} */    
    public void create(Feature fp) {
        assertFeatureNotNull(fp);
        assertFeatureNotExist(fp.getUid());
        updateFeature(fp);
    }

    /** {@inheritDoc} */    
    public void update(Feature fp) {
        assertFeatureNotNull(fp);
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
    public void delete(String uid) {
        assertFeatureExist(uid);
        featuresMap.remove(uid);
        buildGroupsFromFeatures();
    }

    /** {@inheritDoc} */
    public void grantRoleOnFeature(String uid, String roleName) {
        assertFeatureExist(uid);
        assertHasLength(roleName);
        featuresMap.get(uid).getPermissions().add(roleName);
    }

    /** {@inheritDoc} */
    public void removeRoleFromFeature(String uid, String roleName) {
        assertFeatureExist(uid);
        assertHasLength(roleName);
        featuresMap.get(uid).getPermissions().remove(roleName);
    }

    /** {@inheritDoc} */
    public boolean exist(String uid) {
        assertHasLength(uid);
        return featuresMap.containsKey(uid);
    }

    /** {@inheritDoc} */
    public void enable(String uid) {
        assertFeatureExist(uid);
        featuresMap.get(uid).enable();
    }

    /** {@inheritDoc} */    
    public void disable(String uid) {
        assertFeatureExist(uid);
        featuresMap.get(uid).disable();
    }

    /** {@inheritDoc} */
    public Feature read(String uid) {
        assertFeatureExist(uid);
        return featuresMap.get(uid);
    }

    /** {@inheritDoc} */
    public boolean existGroup(String groupName) {
        assertHasLength(groupName);
        return featureGroups.containsKey(groupName);
    }

    /** {@inheritDoc} */
    public void enableGroup(String groupName) {
        assertGroupExist(groupName);
        for (String feat : featureGroups.get(groupName)) {
            this.enable(feat);
        }
    }

    /** {@inheritDoc} */
    public void disableGroup(String groupName) {
        assertGroupExist(groupName);
        for (String feat : featureGroups.get(groupName)) {
            this.disable(feat);
        }
    }

    /** {@inheritDoc} */
    public Map<String, Feature> readGroup(String groupName) {
        assertGroupExist(groupName);
        // Retrieve feature per feature (in-memory, no overhead)
        Map<String, Feature> features = new HashMap<String, Feature>();
        for (String feat : featureGroups.get(groupName)) {
            features.put(feat, this.read(feat));
        }
        return features;
    }

    /** {@inheritDoc} */
    public Set<String> readAllGroups() {
        Set<String> groups = new HashSet<String>();
        groups.addAll(featureGroups.keySet());
        groups.remove(null);
        groups.remove("");
        return groups;
    }

    /** {@inheritDoc} */
    public void addToGroup(String uid, String groupName) {
        assertHasLength(uid);
        assertHasLength(groupName);        
        Feature feat = read(uid);
        feat.setGroup(groupName);
        update(feat);
    }

    /** {@inheritDoc} */
    public void removeFromGroup(String uid, String groupName) {
        assertFeatureExist(uid);
        assertGroupExist(groupName);
        Feature feat = read(uid);
        feat.setGroup("");
        update(feat);
    }
    
    /** {@inheritDoc} */
    @Override
    public void clear() {
       featuresMap.clear();
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        return featuresMap;
    }

    /** {@inheritDoc} */
    @Override
    public String toJson() {
        String json = super.toJson();
        // Remove last } to enrich the json document
        json = json.substring(0, json.length() - 1) + ",\"xmlInputFile\":";
        // No filename inputstream, set to true)
        if (null == fileName) {
            json += "null";
        } else {
            json += "\"" + this.fileName + "\"";
        }
        json += "}";
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
    
}
