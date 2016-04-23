package org.ff4j.web.api.test.filter;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.IOException;

import javax.ws.rs.WebApplicationException;

import org.ff4j.utils.Util;
import org.ff4j.web.ApiConfig;
import org.ff4j.web.api.security.FF4jSecurityContextFilter;
import org.junit.Assert;
import org.junit.Test;

import com.sun.jersey.spi.container.ContainerRequest;


/*
 * #%L
 * ff4j-webapi-jersey1x
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


public class SecurityFilterTest {
    
    @Test
    public void testContextFilterGetWadl() {
        FF4jSecurityContextFilter fscf = new FF4jSecurityContextFilter();
        ContainerRequest mockRequest = mock(ContainerRequest.class);
        when(mockRequest.getMethod()).thenReturn("GET");
        when(mockRequest.getPath(true)).thenReturn("application.wadl");
        fscf.filter(mockRequest);
    }
    
    @Test(expected = WebApplicationException.class)
    public void testUnAuthorized1NoAuthorization() throws IOException {
        // Given
        FF4jSecurityContextFilter faf = new FF4jSecurityContextFilter();
        ContainerRequest mockRequest = mock(ContainerRequest.class);
        when(mockRequest.getMethod()).thenReturn("GET");
        when(mockRequest.getPath(true)).thenReturn("someURLl");
        Assert.assertNotNull(faf);
        faf.filter(mockRequest);
    }
    
    @Test(expected = WebApplicationException.class)
    public void testUnAuthorizedInvalidValue() throws IOException {
        FF4jSecurityContextFilter faf = new FF4jSecurityContextFilter();
        ContainerRequest mockRequest = mock(ContainerRequest.class);
        when(mockRequest.getMethod()).thenReturn("GET");
        when(mockRequest.getPath(true)).thenReturn("someURLl");
        when(mockRequest.getHeaderValue("Authorization")).thenReturn("12");
        Assert.assertNotNull(faf);
        faf.filter(mockRequest);
    }
    
    @Test(expected = WebApplicationException.class)
    public void testUnAuthorizedApiKey() throws IOException {
        FF4jSecurityContextFilter faf = new FF4jSecurityContextFilter();
        ContainerRequest mockRequest = mock(ContainerRequest.class);
        when(mockRequest.getMethod()).thenReturn("GET");
        when(mockRequest.getPath(true)).thenReturn("someURLl");
        when(mockRequest.getHeaderValue("Authorization")).thenReturn("apiKey=12");
        FF4jSecurityContextFilter.setSecurityConfig(new ApiConfig());
        Assert.assertNotNull(faf);
        faf.filter(mockRequest);
    }
    
    @Test(expected = WebApplicationException.class)
    public void testNoAuthorizationAttributeInHeader() throws IOException {
        FF4jSecurityContextFilter faf = new FF4jSecurityContextFilter();
        ContainerRequest mockRequest = mock(ContainerRequest.class);
        when(mockRequest.getMethod()).thenReturn("GET");
        when(mockRequest.getPath(true)).thenReturn("someURLl");
        when(mockRequest.getHeaderValue("Authorization")).thenReturn(null);
        Assert.assertNotNull(faf);
        faf.filter(mockRequest);
    }
    
    @Test
    public void testAuthorizedApiKey() throws IOException {
        // Define ApiConfig
        FF4jSecurityContextFilter.setSecurityConfig(new ApiConfig().createApiKey("12", true, true, Util.set("USER")));
        
        // Given
        FF4jSecurityContextFilter faf = new FF4jSecurityContextFilter();
        ContainerRequest mockRequest = mock(ContainerRequest.class);
        when(mockRequest.getMethod()).thenReturn("GET");
        when(mockRequest.getPath(true)).thenReturn("someURLl");
        when(mockRequest.getHeaderValue("Authorization")).thenReturn("apiKey=12");
        Assert.assertNotNull(faf);
        faf.filter(mockRequest);
    }
    
    
    
    

}
