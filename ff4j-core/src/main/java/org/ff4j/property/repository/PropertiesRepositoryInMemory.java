package org.ff4j.property.repository;

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
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.ff4j.parser.ConfigurationFileParser;
import org.ff4j.parser.FF4jConfigFile;
import org.ff4j.parser.xml.XmlParserV2;
import org.ff4j.property.Property;
import org.ff4j.test.AssertUtils;
/**
 * Implementation of {@link PropertiesRepository} to keep properties in memory.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class PropertiesRepositoryInMemory extends PropertiesRepositorySupport {

    /** serialVersionUID. */
    private static final long serialVersionUID = 5829690784801420235L;

    /** InMemory Property Map */
    private Map<String, Property<?>> mapOfProperties = new LinkedHashMap<>();
    
    /** Default constructor. */
    public PropertiesRepositoryInMemory() {}
    
    /**
     * Provide an xml file to initialize.
     *
     * @param fileName
     *          target fileName
     */
    public PropertiesRepositoryInMemory(String fileName) {
        this(new XmlParserV2(), fileName);
    }
    
    /**
     * Provide an xml file to initialize.
     *
     * @param fileName
     *          target fileName
     */
    public PropertiesRepositoryInMemory(InputStream inputStream) {
        this(new XmlParserV2(), inputStream);
    }
    
    /**
     * Load data with a Parser and a fileName.
     *
     * @param parser
     *      target parser
     * @param fileName
     *      target file name
     */
    public PropertiesRepositoryInMemory(ConfigurationFileParser parser, String fileName) {
        AssertUtils.assertHasLength(fileName, "fileName");
        AssertUtils.assertNotNull(parser,     "parser");
        initWithConfig(parser.parse(fileName));
    }
    
    /**
     * Load data with a Parser and a fileName.
     *
     * @param parser
     *      target parser
     * @param fileName
     *      target file name
     */
    public PropertiesRepositoryInMemory(ConfigurationFileParser parser, InputStream in) {
        AssertUtils.assertNotNull(parser,  "parser");
        AssertUtils.assertNotNull(in, "inputStream");
        initWithConfig(parser.parse(in));
    }
    
    /**
     * Constructor with inputstream fileName.
     * 
     * @param fileName
     *            fileName present in classPath or on fileSystem.
     */
    public PropertiesRepositoryInMemory(FF4jConfigFile ff4jConfig) {
        initWithConfig(ff4jConfig);
    }

    /**
     * Constructor with features to be imported immediately.
     * 
     * @param features
     *      collection of features to be created
     */
    public PropertiesRepositoryInMemory(Collection<Property<?>> properties) {
        initWithProperties(properties);
    }
    
    /**
     * Initialization of features and groups.
     * 
     * @param features
     */
    private void initWithConfig(FF4jConfigFile ff4jConfig) {
        AssertUtils.assertNotNull(ff4jConfig);
        AssertUtils.assertNotNull(ff4jConfig.getProperties());
        mapOfProperties.clear();
        initWithProperties(ff4jConfig.getProperties().values());
    }
    
    /**
     * Initialization of features and groups.
     * 
     * @param features
     */
    private void initWithProperties(Collection<Property<?>> properties) {
        createSchema();
        if (null != properties) {
            this.mapOfProperties = properties.stream().collect(
                    Collectors.toMap(Property::getUid, Function.identity()));
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean exists(String name) {
        assertHasLength(name);
        return mapOfProperties.containsKey(name);
    }
    
    /** {@inheritDoc} */
    @Override
    public Optional < Property<?> > find(String uid) {
        assertHasLength(uid);
        return Optional.ofNullable(mapOfProperties.get(uid));
    }
    
    /** {@inheritDoc} */
    public void saveProperty(Property<?> value) {
        assertPropertyNotNull(value);
        mapOfProperties.put(value.getUid(), value);
    }
    
    /** {@inheritDoc} */
    public void deleteProperty(String name) {
        assertPropertyExist(name);
        mapOfProperties.remove(name);
    }
    
    /** {@inheritDoc} */
    @Override
    public Stream<String> findAllIds() {
        if (mapOfProperties == null) return null;
        return mapOfProperties.keySet().stream();
    }

    /** {@inheritDoc} */
    @Override
    public Stream<Property<?>> findAll() {
        if (mapOfProperties == null) return null;
        return mapOfProperties.values().stream();
    }
    
    /**
     * Setter accessor for attribute 'properties'.
     * @param properties
     * 		new value for 'properties '
     */
    public void setProperties(Map<String, Property<?>> properties) {
        this.mapOfProperties = properties;
    }     
}
