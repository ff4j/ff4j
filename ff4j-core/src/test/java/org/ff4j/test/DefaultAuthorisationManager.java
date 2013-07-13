package org.ff4j.test;

import java.util.HashSet;
import java.util.Set;

import org.ff4j.security.AuthorizationsManager;

public class DefaultAuthorisationManager implements AuthorizationsManager {

	@Override
	public Set<String> getAuthenticatedUserRoles() {
		return new HashSet<String>();
	}

	@Override
	public Set<String> getEveryOneRoles() {
		return new HashSet<String>();
	}

}
