package org.ff4j.inmemory.repository;

import static org.ff4j.test.AssertUtils.assertHasLength;
import static org.ff4j.test.AssertUtils.assertNotNull;
import static org.ff4j.utils.Util.setOf;

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
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.ff4j.conf.XmlParserV1;
import org.ff4j.feature.Feature;
import org.ff4j.feature.AbstractRepositoryFeatures;
import org.ff4j.test.AssertUtils;

/**
 * Storing states of feature inmemory with initial values. Could be used mostly for testing purpose.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class RepositoryFeaturesInMemory extends AbstractRepositoryFeatures {

    /** serialVersionUID. */
    private static final long serialVersionUID = -3768339754263659120L;

    /** XML File where features are load. */
    private String fileName = null;

    /** InMemory Feature Map */
    private Map<String, Feature> mapOfFeatures = new LinkedHashMap<>();

    /** Group structure for features. */
    private Map<String, Set<String>> mapOfGroups = new HashMap<String, Set<String>>();

    /** Default constructor. */
    public RepositoryFeaturesInMemory() {}

    /**
     * Constructor with configuration fileName.
     * 
     * @param fileName
     *            fileName present in classPath or on fileSystem.
     */
    public RepositoryFeaturesInMemory(String fileName) {
        AssertUtils.assertHasLength(fileName);
        createSchema();
        loadConfFile(fileName);
    }

    /**
     * Constructor with inputstream fileName.
     * 
     * @param fileName
     *            fileName present in classPath or on fileSystem.
     */
    public RepositoryFeaturesInMemory(InputStream xmlIN) {
        createSchema();
        loadConf(xmlIN);
    }

    /**
     * Constructor with features to be imported immediately.
     * 
     * @param features
     *      collection of features to be created
     */
    public RepositoryFeaturesInMemory(Collection<Feature> features) {
        createSchema();
        if (null != features) {
            this.mapOfFeatures = features.stream().collect(
                    Collectors.toMap(Feature::getUid, Function.identity()));
            buildGroupsFromFeatures();
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean exists(String uid) {
        assertHasLength(uid);
        return mapOfFeatures.containsKey(uid);
    }
    
    /** {@inheritDoc} */
    @Override
    public Optional < Feature > findById(String uid) {
        assertHasLength(uid);
        return Optional.ofNullable(mapOfFeatures.get(uid));
    }
    
    /** {@inheritDoc} */ 
    @Override
    public void updateFeature(Feature fp) {
        assertNotNull(fp);
        mapOfFeatures.put(fp.getUid(), fp);
        buildGroupsFromFeatures();
    }
    
    /** {@inheritDoc} */    
    @Override
    public void createFeature(Feature fp) {
        updateFeature(fp);
    }
    
    /** {@inheritDoc} */
    @Override
    public void deleteFeature(String uid) {
        assertHasLength(uid);
        mapOfFeatures.remove(uid);
        buildGroupsFromFeatures();
    }
    
    /** {@inheritDoc} */
    @Override
    public void deleteAllFeatures() {
       mapOfFeatures.clear();
    }
    
    /** {@inheritDoc} */
    @Override
    public Stream <Feature> findAll() {
        return mapOfFeatures.values().stream();
    } 
    
    // --- FeatureStore Methods ---
    
    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        assertHasLength(groupName);
        return mapOfGroups.containsKey(groupName);
    }
    
    /** {@inheritDoc} */
    @Override
    public Stream<Feature> readGroup(String groupName) {
        assertGroupExist(groupName);
        return mapOfGroups.get(groupName).stream()
                .map(featureName -> findById(featureName))
                .filter(Optional::isPresent)
                .map(Optional::get)
                .collect(Collectors.toSet())
                .stream();
    }

    /** {@inheritDoc} */
    @Override
    public Stream<String> listAllGroupNames() {
        Set<String> groups = new HashSet<String>();
        groups.addAll(mapOfGroups.keySet());
        groups.remove(null);
        groups.remove("");
        return groups.stream();
    }
    
    // --- Utility Methods ---
    
    /**
     * Load configuration through FF4J.vml file.
     * 
     * @param conf
     *            xml filename
     */
    public void loadConfFile(String conf) {
        assertHasLength(conf);
        this.fileName = conf;
        loadConf(getClass().getClassLoader().getResourceAsStream(conf));
    }

    /**
     * Load configuration through FF4J.vml file.
     * 
     * @param conf
     *            xml filename
     */
    public void loadConf(InputStream xmlIN) {
        if (xmlIN == null) {
            throw new IllegalArgumentException("Cannot parse feature stream");
        }
        this.mapOfFeatures = new XmlParserV1().parseConfigurationFile(xmlIN).getFeatures();
        buildGroupsFromFeatures();
    }

    /**
     * Group is an attribute of the feature and the group structure is rebuild from it.
     */
    private void buildGroupsFromFeatures() {
        
        // Create groups
        mapOfGroups = mapOfFeatures.values().stream()
            .filter(item -> item.getGroup().isPresent())
            .collect(Collectors.< Feature, String, Set<String>>toMap(
                    f -> f.getGroup().get(), 
                    f -> setOf(f.getUid()),
                    //Merged but could we add ?
                    (uid1, uid2) -> { return uid1; }));
        
        // Populate groups
        mapOfFeatures.values().stream()
                .filter(item -> item.getGroup().isPresent())
                .forEach(feature -> mapOfGroups.get(
                         feature.getGroup().get()).add(feature.getUid()));
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

    /** {@inheritDoc} */
    @Override
    public Stream<String> findAllIds() {
        if (mapOfFeatures ==  null) return null;
        return mapOfFeatures.keySet().stream();
    }
    
}
