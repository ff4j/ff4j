package org.ff4j.test;

import java.util.HashSet;
import java.util.Set;

import org.ff4j.Feature;
import org.ff4j.security.AuthorizationsManager;

public class DefaultAuthorisationManager implements AuthorizationsManager {

	@Override
	public boolean isAllowed(Feature fPoint) {
		return true;
	}

	@Override
	public Set<String> getAllUserRoles() {
		return new HashSet<String>();
	}

}
