package org.ff4j.web.api.test.filter;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

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


import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.core.MultivaluedHashMap;
import javax.ws.rs.core.MultivaluedMap;

import org.ff4j.web.api.filter.ApiKey;
import org.ff4j.web.api.filter.ApiKeyValidatorFilter;
import org.junit.Assert;
import org.junit.Test;

public class ApiKeyValidatorFilterTest {
    
    @Test
    public void testApiKeyValidaton() throws Exception {
        Map < String, ApiKey > initMap = new HashMap<>();
        ApiKey ak1 = new ApiKey();
        ak1.setUserId("user1");
        ak1.setValue("key1");
        ak1.setExpirationTime(new Date(System.currentTimeMillis() + 20000));
        initMap.put(ak1.getValue(), ak1);
        new ApiKeyValidatorFilter(initMap);
        Assert.assertTrue(ApiKeyValidatorFilter.getValidApiKeysMap().containsKey("key1"));
        ApiKeyValidatorFilter.setValidApiKeysMap(initMap);
    }
    
    @Test(expected = WebApplicationException.class)
    public void testFilterNondeader() throws Exception {
        ContainerRequestContext mockRequest = mock(ContainerRequestContext.class);
        MultivaluedMap<String, String > mvm = new MultivaluedHashMap<>();
        when(mockRequest.getHeaders()).thenReturn(mvm);
        ApiKeyValidatorFilter f1 = new ApiKeyValidatorFilter();
        f1.filter(mockRequest);
    }
    
    @Test(expected = WebApplicationException.class)
    public void testFilterInvalid() throws Exception {
        ContainerRequestContext mockRequest = mock(ContainerRequestContext.class);
        MultivaluedMap<String, String > mvm = new MultivaluedHashMap<>();
        mvm.putSingle(ApiKeyValidatorFilter.HEADER_APIKEY, "12");
        when(mockRequest.getHeaders()).thenReturn(mvm);
        ApiKeyValidatorFilter f1 = new ApiKeyValidatorFilter();
        f1.filter(mockRequest);
    }
    
    
    @Test(expected = WebApplicationException.class)
    public void testFilterApiKeyNotFound() throws Exception {
        ContainerRequestContext mockRequest = mock(ContainerRequestContext.class);
        MultivaluedMap<String, String > mvm = new MultivaluedHashMap<>();
        mvm.putSingle(ApiKeyValidatorFilter.HEADER_APIKEY, "12");
        when(mockRequest.getHeaders()).thenReturn(mvm);
        
        // create valid KEY to test against
        Map < String, ApiKey > initMap = new HashMap<>();
        ApiKey ak1 = new ApiKey();
        ak1.setUserId("user1");
        ak1.setValue("13");
        ak1.setExpirationTime(new Date(System.currentTimeMillis() + 100000));
        initMap.put(ak1.getValue(), ak1);
        ApiKeyValidatorFilter.setValidApiKeysMap(initMap);
        
        ApiKeyValidatorFilter f1 = new ApiKeyValidatorFilter();
        f1.filter(mockRequest);
    }
    
    @Test(expected = WebApplicationException.class)
    public void testFilterApiKeyExpired() throws Exception {
        ContainerRequestContext mockRequest = mock(ContainerRequestContext.class);
        MultivaluedMap<String, String > mvm = new MultivaluedHashMap<>();
        mvm.putSingle(ApiKeyValidatorFilter.HEADER_APIKEY, "12");
        when(mockRequest.getHeaders()).thenReturn(mvm);
        
        // create valid KEY to test against
        Map < String, ApiKey > initMap = new HashMap<>();
        ApiKey ak1 = new ApiKey();
        ak1.setUserId("user1");
        ak1.setValue("12");
        ak1.setExpirationTime(new Date(System.currentTimeMillis() - 100000));
        initMap.put(ak1.getValue(), ak1);
        ApiKeyValidatorFilter.setValidApiKeysMap(initMap);
        
        ApiKeyValidatorFilter f1 = new ApiKeyValidatorFilter();
        f1.filter(mockRequest);
    }
    
    @Test
    public void testFilterOK() throws Exception {
        ContainerRequestContext mockRequest = mock(ContainerRequestContext.class);
        MultivaluedMap<String, String > mvm = new MultivaluedHashMap<>();
        mvm.putSingle(ApiKeyValidatorFilter.HEADER_APIKEY, "12");
        when(mockRequest.getHeaders()).thenReturn(mvm);
        
        // create valid KEY to test against
        Map < String, ApiKey > initMap = new HashMap<>();
        ApiKey ak1 = new ApiKey();
        ak1.setUserId("user1");
        ak1.setValue("12");
        ak1.setExpirationTime(new Date(System.currentTimeMillis() + 100000));
        initMap.put(ak1.getValue(), ak1);
        ApiKeyValidatorFilter.setValidApiKeysMap(initMap);
        
        ApiKeyValidatorFilter f1 = new ApiKeyValidatorFilter();
        f1.filter(mockRequest);
    }
    
    

}
