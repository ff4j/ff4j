package org.ff4j.security;

import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.Set;

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
	

	@Override
	public Set<String> getAuthenticatedUserRoles() {
		Set<String> listOfRoles = new LinkedHashSet<String>();
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
	public Set<String> getEveryOneRoles() {
		return getAuthenticatedUserRoles();
	}

}