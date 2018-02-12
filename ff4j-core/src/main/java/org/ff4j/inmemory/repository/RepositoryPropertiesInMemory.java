package org.ff4j.inmemory.repository;

import static org.ff4j.test.AssertUtils.assertHasLength;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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

import java.io.InputStream;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Stream;

import org.ff4j.parser.xml.XmlParser;
import org.ff4j.property.AbstractRepositoryProperties;
import org.ff4j.property.Property;
import org.ff4j.property.RepositoryProperties;
import org.ff4j.test.AssertUtils;
/**
 * Implementation of {@link RepositoryProperties} to keep properties in memory.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class RepositoryPropertiesInMemory extends AbstractRepositoryProperties {

    /** serialVersionUID. */
    private static final long serialVersionUID = 5829690784801420235L;

    /** InMemory Map. */
    private RepositoryInMemory<Property<?>> repo = new RepositoryInMemory<>();
   
    /** XML File where features are load. */
    private String fileName = null;
    
    /** InMemory Feature Map */
    private Map<String, Property<?>> properties = new LinkedHashMap<>();
    
    /**
     * Default Constructor 
     */
    public RepositoryPropertiesInMemory() {
    }
    
    /**
     * Constructor with configuration fileName.
     * 
     * @param fileName
     *            fileName present in classPath or on fileSystem.
     */
    public RepositoryPropertiesInMemory(String fileName) {
        repo = new RepositoryInMemory<>(fileName);
    }
    
    /**
     * Constructor with inputstream fileName.
     * 
     * @param fileName
     *            fileName present in classPath or on fileSystem.
     */
    public RepositoryPropertiesInMemory(InputStream xmlIN) {
        loadConf(xmlIN);
    }
    
    /**
     * Read configuration file (property name), retrieve users and populate in memory map.
     */
    private void populateUsersFromXmlConfigurationFile() {
        AssertUtils.assertHasLength(fileName);
        InputStream fileStream = getClass().getClassLoader().getResourceAsStream(fileName);
        populateFromXmlConfigurationStream(fileStream);
    }
    
    /**
     * Read configuration from inputStream.
     * @param xmlIN
     */
    private void populateFromXmlConfigurationStream(InputStream xmlIN) {
        AssertUtils.assertNotNull(xmlIN, "xml Stream");
        // TODO Invoke parser
    }
    
    /**
     * Constructor with full set of feature.
     * 
     * @param maps
     */
    public RepositoryPropertiesInMemory(Map<String, Property<?>> maps) {
        this.properties = maps;
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean exists(String name) {
        assertHasLength(name);
        return properties.containsKey(name);
    }
    
    /** {@inheritDoc} */
    @Override
    public void create(Property<?> value) {
        assertPropertyNotNull(value);
        assertPropertyNotExist(value.getUid());
        properties.put(value.getUid(), value);
    }
    
    /** {@inheritDoc} */
    @Override
    public void delete(String name) {
        assertPropertyExist(name);
        properties.remove(name);
    }
    
    /** {@inheritDoc} */
    @Override
    public Optional < Property<?> > findById(String uid) {
        assertHasLength(uid);
        return Optional.ofNullable(properties.get(uid));
    }
    
    /** {@inheritDoc} */
    @Override
    public Stream<String> findAllIds() {
        if (properties == null) return null;
        return properties.keySet().stream();
    }
    
    /** {@inheritDoc} */
    @Override
    public void deleteAll() {
        properties.clear();
    }

    /** {@inheritDoc} */
    @Override
    public Stream<Property<?>> findAll() {
        if (properties == null) return null;
        return properties.values().stream();
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
       this.properties = XmlParser.parseInputStream(xmlIN).getProperties();
    }
    
    /**
     * Setter accessor for attribute 'properties'.
     * @param properties
     * 		new value for 'properties '
     */
    public void setProperties(Map<String, Property<?>> properties) {
        this.properties = properties;
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
     * @return
     *       current value of 'fileName'
     */
    public String getFileName() {
        return fileName;
    }

    /**
     * Setter accessor for attribute 'fileName'.
     * @param fileName
     * 		new value for 'fileName '
     */
    public void setFileName(String fileName) {
        this.fileName = fileName;
    }
    
}
