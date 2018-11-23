package org.ff4j.user.repo;

import org.ff4j.FF4jRepositoryListener;
import org.ff4j.FF4jRepositorySupport;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2018 FF4J
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

import org.ff4j.event.repo.AuditTrail;
import org.ff4j.user.FF4jUser;
import org.ff4j.user.listener.RepositoryUserListener;

import static org.ff4j.test.AssertUtils.*;

import java.util.Arrays;

/**
 * Superclass as helper to implements user repository.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public abstract class RolesAndUsersRepositorySupport
    extends FF4jRepositorySupport < FF4jUser , FF4jRepositoryListener< FF4jUser >> 
    implements RolesAndUsersRepository {

    /** serialVersionUID. */
    private static final long serialVersionUID = 2472380934533153376L;
    
    /** Listener Name. */
    private static final String LISTENERNAME_AUDIT = "RepositoryUserAuditListener";
    
    /** Listener Name. */
    protected static final String LISTENERNAME_AUDIT_ROLE = "RepositoryRoleAuditListener";
    
    /** Specialized method for users : CREATE */
    protected abstract void saveUser(FF4jUser user);
    
    /** Specialized method for users : DELETE */
    protected abstract void deleteUser(String userId);
    
    /** Specialized method for users : DELETE ALL */
    protected abstract void deleteAllUsers();
     
    /** {@inheritDoc} */
    @Override
    public void delete(String... uids) {
        assertNotEmpty(uids);
        Arrays.stream(uids).forEach(uid -> {
            assertItemExist(uid);
            deleteUser(uid);
            this.notify(l -> l.onDelete(uid));
        });
    }
    
    /** {@inheritDoc} */
    @Override
    public void deleteAll() {
        deleteAllUsers();
        this.notify(l -> l.onDeleteAll());
    }

    /** {@inheritDoc} */
    @Override
    public void save(FF4jUser... entities) {
        assertNotNull(entities);
        Arrays.stream(entities).forEach(entity -> {
            preUpdate(entity);
            assertItemNotExist(entity.getUid());
            saveUser(entity);
            this.notify(l -> l.onUpdate(entity));
        });
    }

    /** {@inheritDoc} */
    @Override
    public void registerListener(String name, FF4jRepositoryListener<FF4jUser> listener) {
        registerListener(name, (RepositoryUserListener) listener);
    }
    
    /** {@inheritDoc} */
    @Override
    public void registerAuditListener(AuditTrail auditTrail) {
        this.registerListener(LISTENERNAME_AUDIT, new RepositoryUserListener(auditTrail));
    }
    
    /** {@inheritDoc} */
    @Override
    public void unRegisterAuditListener() {
        this.unregisterListener(LISTENERNAME_AUDIT);
    }

}
