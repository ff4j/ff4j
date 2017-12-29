package org.ff4j.inmemory.repository;

/*-
 * #%L
 * ff4j-core
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

import java.io.InputStream;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Stream;

import org.ff4j.security.AbstractRepositoryUsers;
import org.ff4j.security.domain.FF4jUser;
import org.ff4j.test.AssertUtils;

/**
 * Implementation to handle Users in memory.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class RepositoryUsersInMemory extends AbstractRepositoryUsers {

    /** serialVersionUID. */
    private static final long serialVersionUID = 2514550814475735765L;
    
    /** InMemory Feature Map */
    private Map<String, FF4jUser > mapOfUsers = new LinkedHashMap<String, FF4jUser>();

    /** xml configuration fileName. */
    private String fileName;
    
    /**
     * Default constructor.
     */
    public RepositoryUsersInMemory() {
    }
    
    /**
     * Init by providing the initial map of users.
     * 
     * @param maps
     */
    public RepositoryUsersInMemory(Map<String, FF4jUser> maps) {
        this.mapOfUsers = maps;
    }
    
    /**
     * Provide an xml file to initialize.
     *
     * @param fileName
     *          target fileName
     */
    public RepositoryUsersInMemory(String fileName) {
        AssertUtils.assertHasLength(fileName, "fileName");
        setLocation(fileName);
    }
    
    /**
     * Constructor with inputstream fileName.
     * 
     * @param fileName
     *            fileName present in classPath or on fileSystem.
     */
    public RepositoryUsersInMemory(InputStream xmlIN) {
        populateUsersFromXmlConfigurationStream(xmlIN);
    }
    
    /** {@inheritDoc} */
    @Override
    public Stream<String> findAllIds() {
        if (mapOfUsers ==  null) return null;
        return mapOfUsers.keySet().stream();
    }
    
    /**
     * Read configuration file (property name), retrieve users and populate in memory map.
     */
    private void populateUsersFromXmlConfigurationFile() {
        AssertUtils.assertHasLength(fileName);
        InputStream fileStream = getClass().getClassLoader().getResourceAsStream(fileName);
        populateUsersFromXmlConfigurationStream(fileStream);
    }
    
    /**
     * Read configuration from inputStream.
     * @param xmlIN
     */
    private void populateUsersFromXmlConfigurationStream(InputStream xmlIN) {
        AssertUtils.assertNotNull(xmlIN, "xml Stream");
        // TODO Invoke parser
    }
    
    /**
     * Setter accessor for attribute 'users'.
     *
     * @param users
     *      new value for 'users '
     */
    public void setUsers(Map<String, FF4jUser> users) {
        this.mapOfUsers = users;
    }
    
    /**
     * Setter accessor for attribute 'locations'.
     * 
     * @param locations
     *            new value for 'locations '
     */
    public void setLocation(String locations) {
        setFileName(locations);
        populateUsersFromXmlConfigurationFile();
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
     *      new value for 'fileName '
     */
    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

}
