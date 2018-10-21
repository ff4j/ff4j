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
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.ff4j.parser.AbstractConfigurationFileParser;
import org.ff4j.parser.FF4jConfigFile;
import org.ff4j.parser.xml.XmlParserV2;
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
    
    /**
     * Default constructor.
     */
    public RepositoryUsersInMemory() {}
    
    /**
     * Provide an xml file to initialize.
     *
     * @param fileName
     *          target fileName
     */
    public RepositoryUsersInMemory(String fileName) {
        this(new XmlParserV2(), fileName);
    }
    
    /**
     * Provide an xml file to initialize.
     *
     * @param fileName
     *          target fileName
     */
    public RepositoryUsersInMemory(InputStream inputStream) {
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
    public RepositoryUsersInMemory(AbstractConfigurationFileParser parser, String fileName) {
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
    public RepositoryUsersInMemory(AbstractConfigurationFileParser parser, InputStream in) {
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
    public RepositoryUsersInMemory(FF4jConfigFile ff4jConfig) {
        initWithConfig(ff4jConfig);
    }

    /**
     * Constructor with features to be imported immediately.
     * 
     * @param features
     *      collection of features to be created
     */
    public RepositoryUsersInMemory(Collection<FF4jUser> users) {
        initWithUsers(users);
    }
    
    /**
     * Initialization of features and groups.
     * 
     * @param features
     */
    private void initWithConfig(FF4jConfigFile ff4jConfig) {
        AssertUtils.assertNotNull(ff4jConfig);
        AssertUtils.assertNotNull(ff4jConfig.getUsers());
        initWithUsers(ff4jConfig.getUsers().values());
    }
    
    /**
     * Initialization of features and groups.
     * 
     * @param features
     */
    private void initWithUsers(Collection<FF4jUser> users) {
        createSchema();
        if (null != users) {
            this.mapOfUsers = users.stream()
                    .collect(Collectors.toMap(FF4jUser::getUid, Function.identity()));
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public Stream<String> findAllIds() {
        if (mapOfUsers ==  null) return null;
        return mapOfUsers.keySet().stream();
    }

}
