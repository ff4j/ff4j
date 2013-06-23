package org.ff4j.security.test;

import java.util.ArrayList;
import java.util.List;

import org.ff4j.Feature;
import org.ff4j.Flipper;
import org.ff4j.security.SpringSecurityAuthorisationManager;
import org.ff4j.store.FeatureStore;
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

	/*
	 * initializing store 
	 */
	private FeatureStore initStore() throws Exception {
		List < String > rights1  = new ArrayList<String>();
		rights1.add(new String("ROLE_USER"));
		List < String > rights2  = new ArrayList<String>();
		rights2.add(new String("X"));
		rights2.add(new String("Y"));
		List<Feature> listOfFlipPoint = new ArrayList<Feature>();
		listOfFlipPoint.add(new Feature("first",  true,  "description", rights1));
		listOfFlipPoint.add(new Feature("second", false, "description", rights1));
		listOfFlipPoint.add(new Feature("third",  true, "description", rights2));
		listOfFlipPoint.add(new Feature("forth",  true,  "description", rights2));
		List < String > allroles = new ArrayList<String>();
		allroles.addAll(rights1);
		allroles.addAll(rights2);
		return new InMemoryFeatureStore(listOfFlipPoint);
	}
	
	private SecurityContext initSecurityContext() {
		SecurityContext context = new SecurityContextImpl();
		List < GrantedAuthority> listOfRoles = new ArrayList<GrantedAuthority>();
		listOfRoles.add( new GrantedAuthorityImpl("ROLE_USER"));
		User u1 = new User("user1", "user1", true, true, true, true, listOfRoles);
		UsernamePasswordAuthenticationToken token = 
				new UsernamePasswordAuthenticationToken(u1.getUsername(), u1.getPassword(), u1.getAuthorities());
		token.setDetails(u1);
		context.setAuthentication(token);
		return context;
	}
	
	@Before
	public void setUp() throws Exception {
		securityCtx = SecurityContextHolder.getContext();
		SecurityContextHolder.setContext(initSecurityContext());
		Flipper f = new Flipper(initStore());
		f.setAuthorizationsManager(new SpringSecurityAuthorisationManager());
	}

	@After
	public void tearDown() {
		SecurityContextHolder.setContext(securityCtx);
	}

	@Test
	public void testIsAuthenticatedAndAuthorized() {
		Authentication auth = SecurityContextHolder.getContext().getAuthentication();
		Assert.assertTrue(auth.isAuthenticated());
        Assert.assertEquals(1, auth.getAuthorities().size());
        
        // not autorized because bad credential
        Assert.assertFalse(Flipper.isFlipped("third"));
        
        // not autorized because bad credential
        Assert.assertTrue(Flipper.isFlipped("first"));
	}

}
