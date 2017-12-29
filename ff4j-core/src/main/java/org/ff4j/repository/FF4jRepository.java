package org.ff4j.repository;

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
import java.util.Collection;
import java.util.Optional;
import java.util.stream.Stream;

import org.ff4j.FF4jEntity;
import org.ff4j.monitoring.AuditTrail;

/**
 * Super Interface to work with features and properties.
 *
 * @author Cedrick LUNVEN (@clunven)
 *
 * @param <TARGET>
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
    long count();

    /**
     * Delete target entity by its id.
     *
     * @param entityId
     *            target entity
     */
    void delete(ID entityId);

    /**
     * Delete several entities in a single call.
     * 
     * @param entities
     *            target entites to remove
     */
    void delete(Iterable<? extends ENTITY> entities);

    /**
     * Delete an entity.
     * 
     * @param entities
     *            target entites to remove
     */
    void delete(ENTITY entity);

    /**
     * Empty the target repository.
     */
    void deleteAll();

    /**
     * Check if an entity exist or not.
     * 
     * @param id
     *            unique identifier of the id
     * @return
     */
    boolean exists(ID id);

    /**
     * Find One entity by its id.
     * 
     * @param id
     *            target identifier
     * @return entity if exist
     */
    Optional<ENTITY> findById(ID id);
    
    /**
     * Retrieve a subset of store.
     *
     * @param ids
     *            unique identifier
     * @return subset of elements
     */
    Stream<ENTITY> find(Iterable<ID> ids);
    
    /**
     * Retrieve all entities of the stores as a collection.
     *
     * @return entities as an {@link Iterable}
     */
    Stream<ENTITY> findAll();    
    
    /**
     * Retriev all keys for a repository.
     *
     * @return
     *      retrieve key for a repository
     */
    Stream <ID> findAllIds();

    /**
     * Find One entity by its id.
     * 
     * @param id
     *            target identifier
     * @return entity if exist
     */
    ENTITY read(ID id);
    
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
        return findById(id).orElse(defaultValue);
    }

    /**
     * Saves a given entity.
     *
     * @param entity
     * @return
     */
    void create(ENTITY entity);

    /**
     * Saves a given entity.
     *
     * @param entity
     * @return
     */
    void update(ENTITY entity);

    /**
     * Import Item into target repository (override if exist).
     *
     * @param entities
     *            target entities
     */
    void save(Collection<ENTITY> entities);

    /**
     * Tell if a target store is empty
     *
     * @return if the store is empty or not
     */
    boolean isEmpty();

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
