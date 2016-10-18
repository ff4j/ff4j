package org.ff4j.property.store;

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
import java.util.Set;

import org.ff4j.conf.XmlParser;
import org.ff4j.property.Property;
import org.ff4j.utils.Util;

/**
 * Implementation of {@link PropertyStore} to keep properties in memory.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class InMemoryPropertyStore extends AbstractPropertyStore {

    /** InMemory Feature Map */
    private Map<String, Property<?>> properties = new LinkedHashMap<String, Property<?>>();

    /** FileName used to retrieve properties. */
    private String fileName;
    
    /**
     * Default Constructor 
     */
    public InMemoryPropertyStore() {
    }
    
    /**
     * Constructor with configuration fileName.
     * 
     * @param fileName
     *            fileName present in classPath or on fileSystem.
     */
    public InMemoryPropertyStore(String fileName) {
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
    public InMemoryPropertyStore(InputStream xmlIN) {
        loadConf(xmlIN);
    }
    
    /**
     * Constructor with full set of feature.
     * 
     * @param maps
     */
    public InMemoryPropertyStore(Map<String, Property<?>> maps) {
        this.properties = maps;
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
            throw new IllegalArgumentException("Cannot parse stream with properties");
        }
        this.properties = new XmlParser().parseConfigurationFile(xmlIN).getProperties();
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean existProperty(String name) {
        Util.assertHasLength(name);
        return properties.containsKey(name);
    }
    
    /** {@inheritDoc} */
    @Override
    public <T> void createProperty(Property<T> value) {
        // Check Params
        assertPropertyNotNull(value);
        assertPropertyNotExist(value.getName());
        // Create
        properties.put(value.getName(), value);
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> readProperty(String name) {
        assertPropertyExist(name);
        return properties.get(name);
    }
    
    /** {@inheritDoc} */
    @Override
    public void deleteProperty(String name) {
        assertPropertyExist(name);
        // Delete
        properties.remove(name);
    }
    
    /** {@inheritDoc} */
    @Override
    public Set<String> listPropertyNames() {
        if (properties == null) {
             return null;
        }
        return properties.keySet();
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        if (properties != null) {
            properties.clear();
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, Property<?>> readAllProperties() {
       return properties;
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
