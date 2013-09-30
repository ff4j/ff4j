package org.ff4j.test.security;

import java.util.HashSet;
import java.util.Set;

import org.ff4j.security.AuthorizationsManager;

/**
 * Dummy {@link AuthorizationsManager} returning empty list for security tests.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class DefaultAuthorisationManager implements AuthorizationsManager {

    /** {@inheritDoc} */
    @Override
    public Set<String> getAuthenticatedUserRoles() {
        return new HashSet<String>();
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> getEveryOneRoles() {
        return new HashSet<String>();
    }

}
