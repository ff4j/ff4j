package org.ff4j.monitoring;

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

import static org.ff4j.test.AssertUtils.assertNotNull;

import org.ff4j.FF4jEntity;
import org.ff4j.event.Event;
import org.ff4j.event.Event.Action;
import org.ff4j.event.Event.Scope;
import org.ff4j.event.Event.Source;
import org.ff4j.repository.FF4jRepositoryListener;

/**
 * Audit Trail superClass.
 *
 * @author Cedrick LUNVEN  (@clunven)
 */
public abstract class AbstractAuditTrailListener<E extends FF4jEntity<?>> implements FF4jRepositoryListener<E> {

    /** Audit trali reference. */
    protected AuditTrail auditTrail;
    
    /** Current source from ff4j. */
    protected Source source = Source.JAVA_API;
    
    /** Scope for entity. */
    protected Scope scopeEntity = Scope.UNKNOWN;
    
    /** Scope for store. */
    protected Scope scopeStore = Scope.UNKNOWN;
    
    public AbstractAuditTrailListener(AuditTrail auditTrail, Scope sEntity, Scope sStore) {
        this.scopeEntity = sEntity;
        this.scopeStore = sStore;
        this.auditTrail = auditTrail;
    }
    
    protected Event createEvent(Action action, Scope scope) {
        Event evt = new Event().source(source).action(action).scope(scope);
        //populateOwner(evt);
        return evt;
    }
    
    protected void log(Event evt) {
        assertNotNull(evt);
        assertNotNull(auditTrail);
        auditTrail.log(evt);
    }
    
    protected void logEvent(Action action, Scope scope, String uid) {
        log(createEvent(action, scope).targetUid(uid));
    }
    
    /** {@inheritDoc} */
    @Override
    public void onCreateSchema() {
        log(createEvent(Action.CREATE_SCHEMA, scopeStore));
    }
    
    /** {@inheritDoc} */
    public void onDeleteAll() {
        log(createEvent(Action.DELETE, scopeStore));
    }
    
    /** {@inheritDoc} */
    @Override
    public void onUpdate(E entity) {
        logEvent(Action.UPDATE, scopeEntity, entity.getUid());
    }
    
    /** {@inheritDoc} */
    @Override
    public void onCreate(E entity) {
        logEvent(Action.CREATE, scopeEntity, entity.getUid());
    }

    /** {@inheritDoc} */
    @Override
    public void onDelete(String uid) {
        logEvent(Action.DELETE, scopeEntity, uid);
    }   

    protected void onUpdateEntity(E entity) {
        logEvent(Action.UPDATE, scopeEntity, entity.getUid());
    }

}
