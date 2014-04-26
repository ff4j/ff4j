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

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.core.FeatureXmlParser;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;

/**
 * Storing states of feature inmemory with initial values. Could be used mostly for testing purpose.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class InMemoryFeatureStore implements FeatureStore {

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
        loadConfFile(fileName);
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
        InputStream xmlIN = getClass().getClassLoader().getResourceAsStream(conf);
        if (xmlIN == null) {
            throw new IllegalArgumentException("Cannot load file " + conf + " from classpath");
        }
        this.fileName = conf;
        this.featuresMap = new FeatureXmlParser().parseConfigurationFile(xmlIN);
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
    @Override
    public void create(Feature fp) {
        if (exist(fp.getUid())) {
            throw new FeatureAlreadyExistException(fp.getUid());
        }
        updateFeature(fp);
    }

    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        Feature fpExist = read(fp.getUid());
        // Checking new roles
        Set<String> toBeAdded = new HashSet<String>();
        toBeAdded.addAll(fp.getAuthorizations());
        toBeAdded.removeAll(fpExist.getAuthorizations());
        for (String addee : toBeAdded) {
            // Will fail if invalid userrole
            grantRoleOnFeature(fpExist.getUid(), addee);
        }
        updateFeature(fp);
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String fpId) {
        if (!exist(fpId)) {
            throw new FeatureNotFoundException(fpId);
        }
        featuresMap.remove(fpId);
        buildGroupsFromFeatures();
    }

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String flipId, String roleName) {
        if (!exist(flipId)) {
            throw new FeatureNotFoundException(flipId);
        }
        featuresMap.get(flipId).getAuthorizations().add(roleName);
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String flipId, String roleName) {
        if (!exist(flipId)) {
            throw new FeatureNotFoundException(flipId);
        }
        featuresMap.get(flipId).getAuthorizations().remove(roleName);
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String featId) {
        return featuresMap.containsKey(featId);
    }

    /** {@inheritDoc} */
    @Override
    public void enable(String featID) {
        if (!exist(featID)) {
            throw new FeatureNotFoundException(featID);
        }
        featuresMap.get(featID).enable();
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String featID) {
        if (!exist(featID)) {
            throw new FeatureNotFoundException(featID);
        }
        featuresMap.get(featID).disable();
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String featureUid) {
        if (!exist(featureUid)) {
            throw new FeatureNotFoundException(featureUid);
        }
        return featuresMap.get(featureUid);
    }

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        return featureGroups.containsKey(groupName);
    }

    /** {@inheritDoc} */
    @Override
    public void enableGroup(String groupName) {
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
        return groups;
    }

    /** {@inheritDoc} */
    @Override
    public void addToGroup(String featureId, String groupName) {
        if (!exist(featureId)) {
            throw new FeatureNotFoundException(featureId);
        }
        Feature feat = read(featureId);
        feat.setGroup(groupName);
        update(feat);
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String featureId, String groupName) {
        if (!exist(featureId)) {
            throw new FeatureNotFoundException(featureId);
        }
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        Feature feat = read(featureId);
        if (feat.getGroup() != null && !feat.getGroup().equals(groupName)) {
            throw new IllegalArgumentException("'" + featureId + "' is not in group '" + groupName + "'");
        }
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
        return "InMemoryFeatureStore [featuresMap=" + featuresMap + "]";
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
