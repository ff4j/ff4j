package org.ff4j.security;

import java.util.Set;

import org.ff4j.Feature;


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
	 * 		feature point with autorisations.
	 *
	 * @return
	 */
	boolean isAllowed(Feature fPoint);
	
	/**
	 * Retrieves user roles from all users (if available, for spring security it's not available out-of-the-box and should be
	 * overrides to match the userDetails implementation - for instance dedicated sql-query). 
	 * 
	 * @return
	 * 		list of all userroles availables
	 */
	Set < String > getAllUserRoles();
	
}
