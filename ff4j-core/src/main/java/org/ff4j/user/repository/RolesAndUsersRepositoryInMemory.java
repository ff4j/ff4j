package org.ff4j.user.repository;

import static org.ff4j.test.AssertUtils.assertHasLength;
import static org.ff4j.test.AssertUtils.assertNotNull;

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
import java.time.LocalDateTime;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.ff4j.FF4jEntity;
import org.ff4j.exception.ItemNotFoundException;
import org.ff4j.parser.ConfigurationFileParser;
import org.ff4j.parser.FF4jConfigFile;
import org.ff4j.parser.xml.XmlParserV2;
import org.ff4j.test.AssertUtils;
import org.ff4j.user.FF4jRole;
import org.ff4j.user.FF4jUser;
import org.ff4j.user.exception.RoleNotFoundException;

/**
 * Implementation to handle Users in memory.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class RolesAndUsersRepositoryInMemory extends RolesAndUsersRepositorySupport {

    /** serialVersionUID. */
    private static final long serialVersionUID = 2514550814475735765L;
    
    /** InMemory Feature Map */
    private Map<String, FF4jUser > mapOfUsers = new LinkedHashMap<String, FF4jUser>();
    
    /** InMemory Feature Map */
    private Map < String, FF4jRole > mapOfRoles = new LinkedHashMap<String, FF4jRole >();
    
    /**
     * Default constructor.
     */
    public RolesAndUsersRepositoryInMemory() {}
    
    /**
     * Provide an xml file to initialize.
     *
     * @param fileName
     *          target fileName
     */
    public RolesAndUsersRepositoryInMemory(String fileName) {
        this(new XmlParserV2(), fileName);
    }
    
    /**
     * Provide an xml file to initialize.
     *
     * @param fileName
     *          target fileName
     */
    public RolesAndUsersRepositoryInMemory(InputStream inputStream) {
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
    public RolesAndUsersRepositoryInMemory(ConfigurationFileParser parser, String fileName) {
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
    public RolesAndUsersRepositoryInMemory(ConfigurationFileParser parser, InputStream in) {
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
    public RolesAndUsersRepositoryInMemory(FF4jConfigFile ff4jConfig) {
        initWithConfig(ff4jConfig);
    }

    /**
     * Constructor with features to be imported immediately.
     * 
     * @param features
     *      collection of features to be created
     */
    public RolesAndUsersRepositoryInMemory(Collection<FF4jUser> users) {
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
        initWithRoles(ff4jConfig.getRoles().values());
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
    
    /**
     * Initialization of features and groups.
     * 
     * @param features
     */
    private void initWithRoles(Collection<FF4jRole> roles) {
        createSchema();
        if (null != roles) {
            this.mapOfRoles = roles.stream()
                    .collect(Collectors.toMap(FF4jRole::getUid, Function.identity()));
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public Stream<String> findAllIds() {
        if (mapOfUsers ==  null) return null;
        return mapOfUsers.keySet().stream();
    }

    /** {@inheritDoc} */
    @Override
    public long countRoles() {
        return findAllRoles().count();
    }

    /** {@inheritDoc} */
    @Override
    public boolean existsRole(String roleName) {
        assertHasLength(roleName);
        return mapOfRoles.containsKey(roleName);
    }
    
    /** {@inheritDoc} */
    @Override
    public void deleteRole(String roleName) {
        assertNotNull(roleName);
        if (!existsRole(roleName)) {
            throw new RoleNotFoundException(roleName);
        }
        mapOfRoles.remove(roleName);
    }
    
    /** {@inheritDoc} */
    @Override
    public void deleteRole(FF4jRole role) {
        assertNotNull(role);
        this.deleteRole(role.getUid());
    }

    /** {@inheritDoc} */
    @Override
    public void deleteRoles(Iterable<FF4jRole> entities) {
        if (null != entities) entities.forEach(this::deleteRole);
    }
    
    /** {@inheritDoc} */
    @Override
    public void deleteAllRoles() {
        mapOfRoles.clear();
    }
    
    /** {@inheritDoc} */
    @Override
    public Stream<String> listRoleNames() {
        return findAllRoles().map(FF4jEntity::getUid);
    }

    /** {@inheritDoc} */
    @Override
    public Stream<FF4jRole> findRoles(Iterable<String> roleNames) {
        if (roleNames == null) return Stream.empty();
        return StreamSupport
                // Iterable to Stream \_(o^o')_/
                .stream(roleNames.spliterator(),  false)
                // N+1 Select 'find' 
                .map(this::findRole)
                // Get only if found
                .filter(Optional::isPresent)
                // Access data
                .map(Optional::get);
    }
    
    /** {@inheritDoc} */
    @Override
    public FF4jRole readRole(String roleName) {
        if (!existsRole(roleName)) {
            throw new ItemNotFoundException(roleName);
        }
        return findRole(roleName).get();
    }
    
    /** {@inheritDoc} */
    @Override
    public Optional<FF4jRole> findRole(String roleName) {
        return existsRole(roleName) ? 
                Optional.ofNullable(mapOfRoles.get(roleName)) : 
                Optional.empty();
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean isRoleEmpty() {
        return mapOfRoles.isEmpty();
    }
    
    /** {@inheritDoc} */
    @Override
    public void saveRoles(Collection<FF4jRole> entities) {
        if (entities != null) {
            entities.stream().forEach(this::updateRole);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void createRole(FF4jRole entity) {
        assertNotNull(entity);
        assertHasLength(entity.getUid());
        entity.setLastModified(LocalDateTime.now());
        entity.setCreationDate(entity.getCreationDate().orElse(entity.getLastModifiedDate().get()));
        mapOfRoles.put(entity.getUid(), entity);
    }

    /** {@inheritDoc} */
    @Override
    public void updateRole(FF4jRole entity) {
        assertNotNull(entity);
        assertHasLength(entity.getUid());
        entity.setLastModified(LocalDateTime.now());
        mapOfRoles.put(entity.getUid(), entity);
    }
   
    /** {@inheritDoc} */
    @Override
    public boolean exists(String userId) {
        assertHasLength(userId);
        return mapOfUsers.containsKey(userId);
    }

    /** {@inheritDoc} */
    @Override
    public Optional<FF4jUser> find(String userid) {
        return exists(userid) ? 
                Optional.ofNullable(mapOfUsers.get(userid)) : 
                Optional.empty();
    }

    /** {@inheritDoc} */
    @Override
    public Stream<FF4jUser> findAll() {
        return mapOfUsers.values().stream();
    }
  
    /** {@inheritDoc} */
    @Override
    protected void saveUser(FF4jUser user) {
        assertNotNull(user);
        assertHasLength(user.getUid());
        user.setLastModified(LocalDateTime.now());
        mapOfUsers.put(user.getUid(), user);
    }

    /** {@inheritDoc} */
    @Override
    protected  void deleteUser(String userId) {
        assertNotNull(userId);
        assertItemExist(userId);
        mapOfUsers.remove(userId);
    }

    /** {@inheritDoc} */
    @Override
    protected void deleteAllUsers() {
        mapOfUsers.clear();
    }

    /** {@inheritDoc} */
    @Override
    public Stream<FF4jRole> findAllRoles() {
        return mapOfRoles.values().stream();
    }

    /** {@inheritDoc} */
    @Override
    public void save(Iterable<FF4jUser> entities) {
       if (entities != null) {
           entities.forEach(this::saveUser);
       }
    }

    /** {@inheritDoc} */
    @Override
    public void delete(Iterable<String> entities) {
        if (entities != null) {
            entities.forEach(this::deleteUser);
        }
    }

}