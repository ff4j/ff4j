package org.ff4j.user.repository;

import java.util.Collection;
import java.util.Optional;

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

import java.util.stream.Stream;

import org.ff4j.FF4jRepository;
import org.ff4j.user.FF4jRole;
import org.ff4j.user.FF4jUser;

/**
 * Common operations with Users.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public interface RolesAndUsersRepository extends FF4jRepository<String, FF4jUser > {
    
    /**
     * Syntax sugar.
     *
     * @return
     *      list of properties names.
     */
    default Stream < String > listUsersNames() {
        return findAllIds();
    }

    /**
     * Count number of elements in the repository
     *
     * @return target
     */
    long countRoles();

    /**
     * Delete target entity by its id.
     *
     * @param entityId
     *            target entity
     */
    void deleteRole(String roleName);

    /**
     * Delete several entities in a single call.
     * 
     * @param entities
     *            target entites to remove
     */
    void deleteRoles(Iterable<FF4jRole> roles);

    /**
     * Delete an entity.
     * 
     * @param entities
     *            target entites to remove
     */
    void deleteRole(FF4jRole entity);

    /**
     * Empty the target repository.
     */
    void deleteAllRoles();

    /**
     * Check if an entity exist or not.
     * 
     * @param id
     *            unique identifier of the id
     * @return
     */
    boolean existsRole(String roleName);

    /**
     * Find One entity by its id.
     * 
     * @param id
     *            target identifier
     * @return entity if exist
     */
    Optional<FF4jRole> findRole(String roleName);
    
    /**
     * Retrieve a subset of store.
     *
     * @param ids
     *            unique identifier
     * @return subset of elements
     */
    Stream<FF4jRole> findRoles(Iterable<String> roleNames);
    
    /**
     * Retrieve all entities of the stores as a collection.
     *
     * @return entities as an {@link Iterable}
     */
    Stream<FF4jRole> findAllRoles();
    
    /**
     * Retriev all keys for a repository.
     *
     * @return
     *      retrieve key for a repository
     */
    Stream <String> listRoleNames();

    /**
     * Find One entity by its id.
     * 
     * @param id
     *            target identifier
     * @return entity if exist
     */
    FF4jRole readRole(String roleName);
    
    /**
     * Retrieve an entity by id and if not found, give default value.
     *
     * @param id
     *      unique item  identifier
     * @param defaultValue
     *      provide default value
     * @return
     *      target entity
     */
    default FF4jRole readRole(String roleName, FF4jRole defaultValue) {
        return findRole(roleName).orElse(defaultValue);
    }

    /**
     * Saves a given entity.
     *
     * @param entity
     * @return
     */
    void createRole(FF4jRole entity);

    /**
     * Saves a given entity.
     *
     * @param entity
     * @return
     */
    void updateRole(FF4jRole entity);

    /**
     * Import Item into target repository (override if exist).
     *
     * @param entities
     *            target entities
     */
    void saveRoles(Collection<FF4jRole> entities);

    /**
     * Tell if a target store is empty
     *
     * @return if the store is empty or not
     */
    boolean isRoleEmpty();
    
}
