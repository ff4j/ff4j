package org.ff4j;

import static org.ff4j.test.AssertUtils.assertNotEmpty;
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

import java.io.Serializable;
import java.util.Arrays;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.ff4j.event.repository.AuditTrail;
import org.ff4j.exception.ItemNotFoundException;

/**
 * Super Interface to work with features and properties.
 *
 * @author Cedrick LUNVEN (@clunven)
 *
 * @param <ENTITY>
 *            current object to work with
 * @param <ID>
 *            unique identifier (String most of the case).
 * 
 * @since 2.x
 */
public interface FF4jRepository<ID extends Serializable, ENTITY extends FF4jEntity<?>> {

    /**
     * Initialize the target database schema by creating expected structures.
     * 
     * <li>TABLE, INDEX will be created for JDBC, but also COLLECTION and INDEXS for MongoDb, or COLUMN FAMILY for Cassandra.
     * <li>The structures will be created only if they don't exist.
     * <li>In some cases, there is nothing todo (Ehcache, Redis, InMemory), the method won't failed but do nothing (it does not
     * clear the DB)
     * 
     * @since 1.6
     */
    void createSchema();

    /**
     * Count number of elements in the repository
     *
     * @return target
     */
    default long count() {
        return findAll().count();
    }
    
    /**
     * Tell if a target store is empty
     *
     * @return if the store is empty or not
     */
    default boolean isEmpty() {
        return count() == 0;
    }
    
    /**
     * Check if an entity exist or not.
     * 
     * @param id
     *            unique identifier of the id
     * @return
     */
    boolean exists(ID id);
    

    // --------------------------
    //    SAVE (CREATE/UPDATE)
    // --------------------------
    
    /**
     * Save a collection of entities
     */
    void save(Iterable<ENTITY> entities);
    
    /**
     * Save a collection of entities
     */
    default void save(Stream<ENTITY> entities) {
        assertNotNull(entities);
        save(entities.collect(Collectors.toList()));
    }
    
    /**
     * Save a collection of entities
     */
    @SuppressWarnings("unchecked")
    default void save(ENTITY... entities) {
        assertNotEmpty(entities);
        save(Arrays.asList(entities));
    }

    // --------------------------
    //         FIND (READ)
    // --------------------------
    
    /**
     * Retrieve all keys for a repository.
     *
     * @return
     *      retrieve key for a repository
     */
    Stream<ID> findAllIds();
    
    /**
     * Find One entity by its id.
     * 
     * @param id
     *            target identifier
     * @return entity if exist
     */
    Optional<ENTITY> find(ID id);
    
    /**
     * Retrieve a subset of store.
     *
     * @param ids
     *            unique identifier
     * @return subset of elements
     */
    default Stream<ENTITY> find(Iterable<ID> ids) {
        if (ids == null) return Stream.empty();
        return StreamSupport
                // Iterable to Stream \_(o^o')_/
                .stream(ids.spliterator(),  false)
                // N+1 Select 'find' 
                .map(this::find)
                // Get only if found
                .filter(Optional::isPresent)
                // Access data
                .map(Optional::get);
    }
    
    /**
     * Retrieve all entities of the stores as a collection.
     *
     * @return entities as an {@link Iterable}
     */
    default Stream<ENTITY> findAll() {
        return find(findAllIds().collect(Collectors.toList()));
    }

    /**
     * Find One entity by its id.
     * 
     * @param id
     *            target identifier
     * @return entity if exist
     */
     default ENTITY read(ID uid) {
         if (!exists(uid)) {
             throw new ItemNotFoundException(String.valueOf(uid));
         }
         return find(uid).get();
     }
    
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
    default ENTITY read(ID id, ENTITY defaultValue) {
        return find(id).orElse(defaultValue);
    }
    
    // --------------------------
    //         DELETE
    // --------------------------
    
    /**
     * Delete several entities in a single call.
     * 
     * @param entities
     *            target entites to remove
     */
    void delete(Iterable<ID> entities);
    
    /**
     * Delete several entities in a single call.
     * 
     * @param entities
     *            target entites to remove
     */
    default void delete(Stream<ID> entities) {
        assertNotNull(entities);
        delete(entities.collect(Collectors.toList()));
    }
    
    /**
     * Delete target entity by its id.
     *
     * @param entityId
     *            target entity
     */
    @SuppressWarnings("unchecked")
    default void delete(ID... entities) {
        assertNotEmpty(entities);
        delete(Arrays.asList(entities));
    }
    
    /**
     * Empty the target repository.
     */
    default void deleteAll() {
        delete(findAllIds());
    }
    
    // --------------------------
    //         Listeners
    // --------------------------
    
    /**
     * Explicitely register a listener on the CRUD operation (Pattern Observable).
     *
     * @param name
     *            listener name
     * @param listener
     *            listener
     */
    void registerListener(String name, FF4jRepositoryListener<ENTITY> listener);

    /**
     * Enable auditing to the audit trail.
     *
     * @param auditTrail
     *            audit trail destination
     */
    void registerAuditListener(AuditTrail auditTrail);

    /**
     * Enable auditing to the audit trail.
     *
     * @param auditTrail
     *            audit trail destination
     */
    void unRegisterAuditListener();

    /**
     * Explicitely unregister a listener by its unique name.
     *
     * @param name
     *            listener name
     */
    void unregisterListener(String name);

}
