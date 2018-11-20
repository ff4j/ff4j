package org.ff4j.user.repo;

import org.ff4j.event.repo.AuditTrail;
import org.ff4j.repository.FF4jRepositoryListener;
import org.ff4j.repository.FF4jRepositorySupport;
import org.ff4j.user.FF4jUser;
import org.ff4j.user.listener.RepositoryUserListener;

/**
 * Superclass as helper to implements user repository.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public abstract class RepositoryRolesAndUsersSupport
    extends FF4jRepositorySupport < FF4jUser , FF4jRepositoryListener< FF4jUser >> 
    implements RepositoryRolesAndUsers {

    /** serialVersionUID. */
    private static final long serialVersionUID = 2472380934533153376L;
    
    /** Listener Name. */
    private static final String LISTENERNAME_AUDIT = "RepositoryUserAuditListener";
    
    /** Listener Name. */
    protected static final String LISTENERNAME_AUDIT_ROLE = "RepositoryRoleAuditListener";
    
    /** Specialized method for users : CREATE */
    protected abstract void createUser(FF4jUser user);
    
    /** Specialized method for users : UPDATE */
    protected abstract void updateUser(FF4jUser user);
    
    /** Specialized method for users : DELETE */
    protected abstract void deleteUser(String userId);
    
    /** Specialized method for users : DELETE ALL */
    protected abstract void deleteAllFeatures();
    
    /** {@inheritDoc} */
    @Override
    public void delete(String uid) {
        assertItemExist(uid);
        deleteUser(uid);
        this.notify(l -> l.onDelete(uid));
    }
    
    /** {@inheritDoc} */
    @Override
    public void update(FF4jUser user) {
        preUpdate(user);
        updateUser(user);
        // Notify all listeners registered (like AuditTrail)
        this.notify(l -> l.onUpdate(user));
    }
    
    /** {@inheritDoc} */
    @Override
    public void deleteAll() {
        deleteAllFeatures();
        this.notify(l -> l.onDeleteAll());
    }

    /** {@inheritDoc} */
    @Override
    public void create(FF4jUser entity) {
        preUpdate(entity);
        assertItemNotExist(entity.getUid());
        createUser(entity);
        this.notify(l -> l.onCreate(entity));
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
