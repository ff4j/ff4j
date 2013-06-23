package org.ff4j.security;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import org.ff4j.Feature;
import org.ff4j.security.AuthorizationsManager;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;

/**
 * Implementation of {@link AuthorizationsManager} with SpringSecurity framework.
 * @author clunven
 *
 */
public class SpringSecurityAuthorisationManager implements AuthorizationsManager {

	/** Logger for Advisor. */
	final static Logger LOG = LoggerFactory.getLogger(SpringSecurityAuthorisationManager.class);
	
	/**
	 * Log Once the warning
	 */
	public SpringSecurityAuthorisationManager() {
		LOG.warn("Only user roles are loaded - cannot retrieve roles of EVERYONE as a list");
	}
	
	/** {@inheritDoc} */
	public boolean isAllowed(Feature featureName) {
		// Load Spring Security GrantedAuthorities
		List<String> userRoles = getAuthenticatedUserRoles();
		// Filter with expected roles
		for (String expectedRole : featureName.getAuthorizations()) {
			if (userRoles.contains(expectedRole)) return true;
		}
		return false;
	}

	/**
	 * Retrieve user roles.
	 * @return
	 * 		roles of authenticated user.
	 */
	private List<String> getAuthenticatedUserRoles() {
		List<String> listOfRoles = new ArrayList<String>();
		Authentication auth = SecurityContextHolder.getContext().getAuthentication();
		if (!(auth instanceof AnonymousAuthenticationToken)) {
			Collection<GrantedAuthority> rights = auth.getAuthorities();
			for (GrantedAuthority grantedAuthority : rights) {
				listOfRoles.add(grantedAuthority.getAuthority());
			}
		}
		return listOfRoles;
	}
	
	/** {@inheritDoc} */
	public Set<String> getAllUserRoles() {
		return new TreeSet<String>(getAuthenticatedUserRoles());
	}

}