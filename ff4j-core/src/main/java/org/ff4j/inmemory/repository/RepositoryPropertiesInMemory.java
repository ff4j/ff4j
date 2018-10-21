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
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.ff4j.parser.AbstractConfigurationFileParser;
import org.ff4j.parser.FF4jConfigFile;
import org.ff4j.parser.xml.XmlParserV2;
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

    /** InMemory Property Map */
    private Map<String, Property<?>> mapOfProperties = new LinkedHashMap<>();
    
    /** Default constructor. */
    public RepositoryPropertiesInMemory() {}
    
    /**
     * Provide an xml file to initialize.
     *
     * @param fileName
     *          target fileName
     */
    public RepositoryPropertiesInMemory(String fileName) {
        this(new XmlParserV2(), fileName);
    }
    
    /**
     * Provide an xml file to initialize.
     *
     * @param fileName
     *          target fileName
     */
    public RepositoryPropertiesInMemory(InputStream inputStream) {
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
    public RepositoryPropertiesInMemory(AbstractConfigurationFileParser parser, String fileName) {
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
    public RepositoryPropertiesInMemory(AbstractConfigurationFileParser parser, InputStream in) {
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
    public RepositoryPropertiesInMemory(FF4jConfigFile ff4jConfig) {
        initWithConfig(ff4jConfig);
    }

    /**
     * Constructor with features to be imported immediately.
     * 
     * @param features
     *      collection of features to be created
     */
    public RepositoryPropertiesInMemory(Collection<Property<?>> properties) {
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
        initWithProperties(ff4jConfig.getProperties().values());
    }
    
    /**
     * Initialization of features and groups.
     * 
     * @param features
     */
    private void initWithProperties(Collection<Property<?>> features) {
        createSchema();
        if (null != features) {
            this.mapOfProperties = features
                    .stream()
                    .collect(Collectors.toMap(Property::getUid, Function.identity()));
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
    public void create(Property<?> value) {
        assertPropertyNotNull(value);
        assertPropertyNotExist(value.getUid());
        mapOfProperties.put(value.getUid(), value);
    }
    
    /** {@inheritDoc} */
    @Override
    public void delete(String name) {
        assertPropertyExist(name);
        mapOfProperties.remove(name);
    }
    
    /** {@inheritDoc} */
    @Override
    public Optional < Property<?> > findById(String uid) {
        assertHasLength(uid);
        return Optional.ofNullable(mapOfProperties.get(uid));
    }
    
    /** {@inheritDoc} */
    @Override
    public Stream<String> findAllIds() {
        if (mapOfProperties == null) return null;
        return mapOfProperties.keySet().stream();
    }
    
    /** {@inheritDoc} */
    @Override
    public void deleteAll() {
        mapOfProperties.clear();
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
