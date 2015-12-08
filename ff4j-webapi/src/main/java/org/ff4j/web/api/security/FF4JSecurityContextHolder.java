package org.ff4j.web.api.security;

import javax.ws.rs.core.SecurityContext;

/**
 * Security Context holder.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FF4JSecurityContextHolder {

    /** Put current security context as threadlocal to be reused by the AuthenticationProvider. */
    private static final ThreadLocal< SecurityContext > securityContextHolder = new ThreadLocal< SecurityContext >();
    
    /**
     * Return custom FF4J Security Context.
     *
     * @return
     */
    public static void save(SecurityContext securityContext) {
        securityContextHolder.set(securityContext);
    }
    
    public static SecurityContext getSecurityContext() {
        return securityContextHolder.get();
    }
    
}
