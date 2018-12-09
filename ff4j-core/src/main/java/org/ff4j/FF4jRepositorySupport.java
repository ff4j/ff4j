package org.ff4j;

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
import static org.ff4j.test.AssertUtils.assertNotNull;

import java.io.Serializable;
import java.time.LocalDateTime;

import org.ff4j.exception.ItemAlreadyExistException;
import org.ff4j.exception.ItemNotFoundException;

/**
 * Proposition of abstraction to perform operations on entities.
 * 
 * - It has been inspired by <a href="http://docs.spring.io/spring-data/commons/docs/current/api/org/springframework/data/repository/CrudRepository.html">
 * spring data crud repository</a> and <a href="http://static.appfuse.org/appfuse-data/appfuse-ibatis/apidocs/org/appfuse/dao/GenericDao.html">App fuse.</a>
 * - GoF Observable pattern allow to send notifications when entities are modified (audit Trail)
 * 
 *
 * @param <V>
 *      entity manipulated, its unique key is a STRING named 'uid'
 */
public abstract class FF4jRepositorySupport < E extends FF4jEntity<?>, LISTENER extends FF4jRepositoryListener < E >> 
                            extends FF4jRepositoryObserver < LISTENER > 
                            implements FF4jRepository<String, E>, Serializable {

    /** Denerated Serial Number . */
    private static final long serialVersionUID = -2865266843791651125L;
    
    /** Json Attribute. */
    public static final String JSON_ATTRIBUTE_CLASSNAME = "className";
   
    /**
     * Controls before updating and update modified date.
     * @param entity
     *      current entity
     */
    protected void preUpdate(E entity) {
        assertNotNull(entity);
        assertHasLength(entity.getUid());
        entity.setLastModified(LocalDateTime.now());
        entity.setCreationDate(entity.getCreationDate().orElse(entity.getLastModifiedDate().get()));
    }
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        // If not overrided, should notify subscriber anyway.
        this.notify(FF4jRepositoryListener::onCreateSchema);
        return;
    }
    
    // ---------------------------------------------------------------
    // ---         Utility methods to work with Repositories       ---
    // ---------------------------------------------------------------
    
    /**
     * Validate feature uid.
     *
     * @param uid
     *      target uid
     */
    protected void assertItemExist(String uid) {
        assertHasLength(uid);
        if (!exists(uid)) {
            throw new ItemNotFoundException(uid);
        }
    }
    
    /**
     * Check that current feature does not exist.
     *
     * @param uid
     *      current feature identifier.s
     */
    protected void assertItemNotExist(String uid) {
        assertHasLength(uid);
        if (exists(uid)) {
            throw new ItemAlreadyExistException(uid);
        }
    }
}
