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

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.core.UriInfo;

import org.ff4j.utils.Util;
import org.ff4j.web.ApiConfig;
import org.ff4j.web.api.security.FF4JSecurityContextAuthenticationManager;
import org.ff4j.web.api.security.FF4jAuthenticationFilter;
import org.junit.Test;

public class SecurityAuthenticationFilterTest {
    
    @Test
    public void testGETWadl() throws IOException {
        // Given
        FF4jAuthenticationFilter faf = new FF4jAuthenticationFilter();
        ContainerRequestContext mockRequest = mock(ContainerRequestContext.class);
        when(mockRequest.getMethod()).thenReturn("GET");
        UriInfo mockUriInfo = mock(UriInfo.class);
        when(mockUriInfo.getPath()).thenReturn("application.wadl");
        when(mockRequest.getUriInfo()).thenReturn(mockUriInfo);
        faf.filter(mockRequest);
    }
    
    
    @Test(expected = WebApplicationException.class)
    public void testUnAuthorized1NoAuthorization() throws IOException {
        // Given
        FF4jAuthenticationFilter faf = new FF4jAuthenticationFilter();
        ContainerRequestContext mockRequest = mock(ContainerRequestContext.class);
        when(mockRequest.getMethod()).thenReturn("GET");
        UriInfo mockUriInfo = mock(UriInfo.class);
        when(mockUriInfo.getPath()).thenReturn("someURL");
        when(mockRequest.getUriInfo()).thenReturn(mockUriInfo);
        faf.filter(mockRequest);
    }
    
    @Test(expected = WebApplicationException.class)
    public void testUnAuthorizedInvalidValue() throws IOException {
        // Given
        FF4jAuthenticationFilter faf = new FF4jAuthenticationFilter();
        ContainerRequestContext mockRequest = mock(ContainerRequestContext.class);
        when(mockRequest.getHeaderString("Authorization")).thenReturn("12");
        
        when(mockRequest.getMethod()).thenReturn("GET");
        UriInfo mockUriInfo = mock(UriInfo.class);
        when(mockUriInfo.getPath()).thenReturn("someURL");
        when(mockRequest.getUriInfo()).thenReturn(mockUriInfo);
        faf.filter(mockRequest);
    }
    
    @Test(expected = WebApplicationException.class)
    public void testUnAuthorizedApiKey() throws IOException {
        // Given
        FF4jAuthenticationFilter faf = new FF4jAuthenticationFilter();
        ContainerRequestContext mockRequest = mock(ContainerRequestContext.class);
        when(mockRequest.getHeaderString("Authorization")).thenReturn("apiKey=12");
        when(mockRequest.getMethod()).thenReturn("GET");
        UriInfo mockUriInfo = mock(UriInfo.class);
        when(mockUriInfo.getPath()).thenReturn("someURL");
        when(mockRequest.getUriInfo()).thenReturn(mockUriInfo);
        // Define ApiConfig
        FF4jAuthenticationFilter.setApiConfig(new ApiConfig());
        
        // When
        faf.filter(mockRequest);
    }
    
    @Test(expected = WebApplicationException.class)
    public void testNoAuthorizationAttributeInHeader() throws IOException {
        // Given
        FF4jAuthenticationFilter faf = new FF4jAuthenticationFilter();
        ContainerRequestContext mockRequest = mock(ContainerRequestContext.class);
        when(mockRequest.getHeaderString("Authorization")).thenReturn(null);
        
        when(mockRequest.getMethod()).thenReturn("GET");
        UriInfo mockUriInfo = mock(UriInfo.class);
        when(mockUriInfo.getPath()).thenReturn("someURL");
        when(mockRequest.getUriInfo()).thenReturn(mockUriInfo);
        faf.filter(mockRequest);
    }

    
    @Test
    public void testAuthorizedApiKey() throws IOException {
        // Define ApiConfig
        FF4jAuthenticationFilter.setApiConfig(new ApiConfig().createApiKey("12", true, true, Util.set("USER")));
        
        // Given
        FF4jAuthenticationFilter faf = new FF4jAuthenticationFilter();
        ContainerRequestContext mockRequest = mock(ContainerRequestContext.class);
        when(mockRequest.getHeaderString("Authorization")).thenReturn("apiKey=12");
        when(mockRequest.getMethod()).thenReturn("GET");
        UriInfo mockUriInfo = mock(UriInfo.class);
        when(mockUriInfo.getPath()).thenReturn("someURL");
        when(mockRequest.getUriInfo()).thenReturn(mockUriInfo);
        
        // When
        faf.filter(mockRequest);
    }
    
    
    @Test
    public void testAuthenticationManager() {
        FF4JSecurityContextAuthenticationManager mnger = new FF4JSecurityContextAuthenticationManager();
        mnger.listAllPermissions();
        mnger.getCurrentUserPermissions();
    }

}
