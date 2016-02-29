package org.ff4j.web.api.security;

/*
 * #%L
 * ff4j-webapi
 * %%
 * Copyright (C) 2013 - 2015 FF4J
 * %%
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */


import javax.ws.rs.core.SecurityContext;

/**
 * Security Context holder.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public final class FF4JSecurityContextHolder {

    /** Put current security context as threadlocal to be reused by the AuthenticationProvider. */
    private static final ThreadLocal< SecurityContext > securityContextHolder = new ThreadLocal< SecurityContext >();
    
    private FF4JSecurityContextHolder() {}
    
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
