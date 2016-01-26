package org.ff4j.web.api.test.filter;

/*
 * #%L
 * ff4j-webapi-jersey2x
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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


import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.lang.reflect.Method;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.container.ResourceInfo;
import javax.ws.rs.core.UriInfo;

import org.ff4j.utils.Util;
import org.ff4j.web.ApiConfig;
import org.ff4j.web.api.security.FF4jAuthorizationFilter;
import org.ff4j.web.api.security.FF4jSecurityContext;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

public class SecurityAuthorizationFilterTest {
    
    private Class<?> targetResource = MockResourceClass.class;
    
    private Method methodDeny;
    
    private Method methodPermit;
    
    private Method methodRole;

    private Method methodNothing;

    @Before
    public void initResources() throws Exception {
        targetResource  = MockResourceClass.class;
        methodDeny      = targetResource.getMethod("denyAll", new Class[]{});
        methodPermit    = targetResource.getMethod("permitAll", new Class[]{});
        methodRole      = targetResource.getMethod("user", new Class[]{});
        methodNothing   = targetResource.getMethod("nothing", new Class[]{});
    }
    
    @Test(expected = WebApplicationException.class)
    public void testDenyAll() throws IOException {
        // Given
        FF4jAuthorizationFilter faf = new FF4jAuthorizationFilter();
        ContainerRequestContext mockRequest = mock(ContainerRequestContext.class);
        UriInfo                 mockUriInfo = mock(UriInfo.class);
        ResourceInfo mockResInfo = mock(ResourceInfo.class);
        when(mockResInfo.getResourceMethod()).thenReturn(methodDeny);
        faf.setInfo(mockResInfo);
        when(mockUriInfo.getPath()).thenReturn("localhost");
        when(mockRequest.getSecurityContext()).thenReturn(new FF4jSecurityContext("user", "", Util.set("USER")));
        when(mockRequest.getUriInfo()).thenReturn(mockUriInfo);
        // When
        faf.filter(mockRequest);
        // Then expecte 403
    }
    
    @Test
    public void testPermitAll() throws IOException {
        // Given
        FF4jAuthorizationFilter faf = new FF4jAuthorizationFilter();
        ContainerRequestContext mockRequest = mock(ContainerRequestContext.class);
        UriInfo                 mockUriInfo = mock(UriInfo.class);
        ResourceInfo mockResInfo = mock(ResourceInfo.class);
        when(mockResInfo.getResourceMethod()).thenReturn(methodPermit);
        faf.setInfo(mockResInfo);
        when(mockUriInfo.getPath()).thenReturn("localhost");
        when(mockRequest.getSecurityContext()).thenReturn(new FF4jSecurityContext("user", "", Util.set("USER")));
        when(mockRequest.getUriInfo()).thenReturn(mockUriInfo);
        // When
        faf.filter(mockRequest);
        // Then expecte 403
    }
    
    @Test
    public void testRoleAllowed() throws IOException {
        // Given
        FF4jAuthorizationFilter faf = new FF4jAuthorizationFilter();
        ContainerRequestContext mockRequest = mock(ContainerRequestContext.class);
        UriInfo                 mockUriInfo = mock(UriInfo.class);
        ResourceInfo mockResInfo = new ResourceInfo() {
            public Method getResourceMethod()  { return methodRole;     }
            public Class<?> getResourceClass() { return targetResource; }
        };
        faf.setInfo(mockResInfo);
        when(mockUriInfo.getPath()).thenReturn("localhost");
        when(mockRequest.getSecurityContext()).thenReturn(new FF4jSecurityContext("user", "", Util.set("USER")));
        when(mockRequest.getUriInfo()).thenReturn(mockUriInfo);
        // When
        faf.filter(mockRequest);
        // Then expecte 403
    }
    
    
    @Test
    public void testRoleNothing() throws IOException {
        // Given
        FF4jAuthorizationFilter faf = new FF4jAuthorizationFilter();
        ContainerRequestContext mockRequest = mock(ContainerRequestContext.class);
        UriInfo                 mockUriInfo = mock(UriInfo.class);
        ResourceInfo mockResInfo = new ResourceInfo() {
            public Method getResourceMethod()  { return methodNothing;     }
            public Class<?> getResourceClass() { return targetResource; }
        };
        faf.setInfo(mockResInfo);
        when(mockUriInfo.getPath()).thenReturn("localhost");
        when(mockRequest.getSecurityContext()).thenReturn(new FF4jSecurityContext("user", "", Util.set("USER")));
        when(mockRequest.getUriInfo()).thenReturn(mockUriInfo);
        // When
        faf.filter(mockRequest);
        // OK
    }
    

    @Test(expected = WebApplicationException.class)
    public void testRoleAllowedInvalid() throws IOException {
        // Given
        FF4jAuthorizationFilter faf = new FF4jAuthorizationFilter();
        ContainerRequestContext mockRequest = mock(ContainerRequestContext.class);
        UriInfo                 mockUriInfo = mock(UriInfo.class);
        ResourceInfo mockResInfo = new ResourceInfo() {
            public Method getResourceMethod()  { return methodRole;     }
            public Class<?> getResourceClass() { return targetResource; }
        };
        faf.setInfo(mockResInfo);
        when(mockUriInfo.getPath()).thenReturn("localhost");
        when(mockRequest.getSecurityContext()).thenReturn(new FF4jSecurityContext("user", "", Util.set("OTHER")));
        when(mockRequest.getUriInfo()).thenReturn(mockUriInfo);
        // When
        faf.filter(mockRequest);
        // Then expecte 403
    }
    
    @Test
    public void testAccessors() throws IOException {
        // Given
        FF4jAuthorizationFilter faf = new FF4jAuthorizationFilter();
        Assert.assertNull(faf.getInfo());
        Assert.assertNull(FF4jAuthorizationFilter.getApiConfig());
        FF4jAuthorizationFilter.setApiConfig(new ApiConfig());
        Assert.assertNotNull(FF4jAuthorizationFilter.getApiConfig());
    }
}
