package org.ff4j.security;

import java.util.Set;

/**
 * Allow flipping only if user is allowed to do so.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public interface AuthorizationsManager {

    /**
     * Retrieves current autorization from context.
     * 
     * @param fPoint
     *            feature point with autorisations.
     * 
     * @return
     */
    Set<String> getAuthenticatedUserRoles();

    /**
     * Retrieves user roles from all users (if available, for spring security it's not available out-of-the-box and should be
     * overrides to match the userDetails implementation - for instance dedicated sql-query).
     * 
     * @return list of all userroles availables
     */
    Set<String> getEveryOneRoles();

}
