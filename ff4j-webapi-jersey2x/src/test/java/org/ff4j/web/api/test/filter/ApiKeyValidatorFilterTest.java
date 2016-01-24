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


import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import org.ff4j.web.api.filter.ApiKey;
import org.ff4j.web.api.filter.ApiKeyValidatorFilter;
import org.junit.Assert;
import org.junit.Test;

public class ApiKeyValidatorFilterTest {
    
    @Test
    public void testApiKeyValidato() {
        ApiKeyValidatorFilter f1 = new ApiKeyValidatorFilter();
        
        Map < String, ApiKey > initMap = new HashMap<>();
        ApiKey ak1 = new ApiKey();
        ak1.setUserId("user1");
        ak1.setValue("key1");
        ak1.setExpirationTime(new Date(System.currentTimeMillis() + 20000));
        initMap.put(ak1.getUserId(), ak1);
        f1 = new ApiKeyValidatorFilter(initMap);
        
        Assert.assertTrue(ApiKeyValidatorFilter.getValidApiKeysMap().containsKey("user1"));
        
        
        
    }

}
