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

import static org.ff4j.test.AssertUtils.assertHasLength;

import java.io.InputStream;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

import org.ff4j.parser.AbstractConfigurationFileParser;
import org.ff4j.parser.FF4jConfigFile;
import org.ff4j.parser.xml.XmlParserV2;
import org.ff4j.security.RepositoryAccessControlLists;
import org.ff4j.security.domain.FF4jAcl;
import org.ff4j.test.AssertUtils;

/**
 * Default implementation of {@link RepositoryAccessControlLists} to work with an inmemory map.
 *
 * @author Cedrick LUNVEN  (@clunven)
 */
public class RepositoryAclsInMemory implements RepositoryAccessControlLists, Serializable {
    
    /** serialVersionUID. */
    private static final long serialVersionUID = -323450837207168425L;
    
    /** Holder for different {@link FF4jAcl}. */
    private Map < String , FF4jAcl > mapOfAcl = new HashMap<>();
    
    /**
     * Default constructor.
     */
    public RepositoryAclsInMemory() {}
    
    /**
     * Provide an xml file to initialize.
     *
     * @param fileName
     *          target fileName
     */
    public RepositoryAclsInMemory(String fileName) {
        this(new XmlParserV2(), fileName);
    }
    
    /**
     * Provide an xml file to initialize.
     *
     * @param fileName
     *          target fileName
     */
    public RepositoryAclsInMemory(InputStream inputStream) {
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
    public RepositoryAclsInMemory(AbstractConfigurationFileParser parser, String fileName) {
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
    public RepositoryAclsInMemory(AbstractConfigurationFileParser parser, InputStream in) {
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
    public RepositoryAclsInMemory(FF4jConfigFile ff4jConfig) {
        initWithConfig(ff4jConfig);
    }
    
    /**
     * Initialization of features and groups.
     * 
     * @param features
     */
    private void initWithConfig(FF4jConfigFile ff4jConfig) {
        AssertUtils.assertNotNull(ff4jConfig);
        AssertUtils.assertNotNull(ff4jConfig.getAcls());
        initWithAcls(ff4jConfig.getAcls());
    }
    
    /**
     * Initialization of features and groups.
     * 
     * @param features
     */
    private void initWithAcls(Map<String, FF4jAcl> acls) {
        if (null != acls) {
            this.mapOfAcl = acls;
        }
    }
    
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {}

    /** {@inheritDoc} */
    @Override
    public FF4jAcl getAccessControlList(String targetUid) {
        assertHasLength(targetUid);
        if (!mapOfAcl.containsKey(targetUid)) {
            // Never return null
            mapOfAcl.put(targetUid, new FF4jAcl());
        }
        return mapOfAcl.get(targetUid);
    }

    /** {@inheritDoc} */
    @Override
    public void saveAccessControlList(FF4jAcl acl, String targetUid) {
        mapOfAcl.put(targetUid, acl);
    }

}