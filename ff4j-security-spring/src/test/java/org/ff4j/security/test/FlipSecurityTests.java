package org.ff4j.security.test;

import java.util.ArrayList;
import java.util.List;

import org.ff4j.FF4j;
import org.ff4j.security.SpringSecurityAuthorisationManager;
import org.ff4j.store.InMemoryFeatureStore;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.GrantedAuthorityImpl;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.context.SecurityContextImpl;
import org.springframework.security.core.userdetails.User;


/**
 * Testing security context.
 *
 * @author clunven
 */
public class FlipSecurityTests {
	
	/** Security context. */
	private SecurityContext securityCtx;
	
	@Before
	public void setUp() throws Exception {
		securityCtx = SecurityContextHolder.getContext();
		// Init SpringSecurity Context
		SecurityContext context = new SecurityContextImpl();
		List < GrantedAuthority> listOfRoles = new ArrayList<GrantedAuthority>();
		listOfRoles.add( new GrantedAuthorityImpl("ROLE_USER"));
		User u1 = new User("user1", "user1", true, true, true, true, listOfRoles);
		UsernamePasswordAuthenticationToken token = new UsernamePasswordAuthenticationToken(u1.getUsername(), u1.getPassword(), u1.getAuthorities());
		token.setDetails(u1);
		context.setAuthentication(token);
		SecurityContextHolder.setContext(context);
		// <--
	}

	@After
	public void tearDown() {
		SecurityContextHolder.setContext(securityCtx);
	}

	@Test
	public void testIsAuthenticatedAndAuthorized() {
		
		// check authentication	
		Authentication auth = SecurityContextHolder.getContext().getAuthentication();
		Assert.assertTrue(auth.isAuthenticated());
		
		// init
		new FF4j(new InMemoryFeatureStore(), new SpringSecurityAuthorisationManager());
        // not autorized because bad credential
        Assert.assertFalse(FF4j.isFlipped("third"));
        // autorized because role ROLE_USER
        Assert.assertTrue(FF4j.isFlipped("first"));
	}

}
