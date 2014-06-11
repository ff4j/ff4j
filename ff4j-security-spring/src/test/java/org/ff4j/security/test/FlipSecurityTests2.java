package org.ff4j.security.test;

/*
 * #%L FlipSecurityTests2.java (ff4j-security-spring) by Cedrick LUNVEN %% Copyright (C) 2013 Ff4J %% Licensed under the Apache
 * License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of
 * the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import java.util.ArrayList;
import java.util.List;

import org.ff4j.FF4j;
import org.ff4j.security.SpringSecurityAuthorisationManager;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.context.SecurityContextImpl;
import org.springframework.security.core.userdetails.User;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * Testing security context.
 * 
 * @author clunven
 */
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations = {"classpath:*applicationContext-ff4j-security.xml"})
public class FlipSecurityTests2 {

    /** FF4J instance. */
    private FF4j ff4j;

    /** Security context. */
    private SecurityContext securityCtx;

    @Before
    public void setUp() throws Exception {
        securityCtx = SecurityContextHolder.getContext();
        // Init SpringSecurity Context
        SecurityContext context = new SecurityContextImpl();
        List<GrantedAuthority> listOfRoles = new ArrayList<GrantedAuthority>();
        listOfRoles.add(new SimpleGrantedAuthority("ROLE_USER"));
        User u1 = new User("user1", "user1", true, true, true, true, listOfRoles);
        UsernamePasswordAuthenticationToken token = new UsernamePasswordAuthenticationToken(u1.getUsername(), u1.getPassword(),
                u1.getAuthorities());
        token.setDetails(u1);
        context.setAuthentication(token);
        SecurityContextHolder.setContext(context);
        // <--

        ff4j = new FF4j("test-ff4j-security-spring.xml");
        ff4j.setAuthorizationsManager(new SpringSecurityAuthorisationManager());
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
        // autorized because role ROLE_USER
        Assert.assertTrue(ff4j.check("first"));

        // not autorized because bad credential
        Assert.assertFalse(ff4j.check("third"));

    }

}
