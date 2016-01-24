package org.ff4j.web.api.test;

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

import org.ff4j.web.api.filter.ApiKey;
import org.junit.Assert;
import org.junit.Test;


public class ApiKeyBeanTest {
    
    @Test
    public void ApiKeyinit() {
        ApiKey ak = new ApiKey();
        ak.setUserId("uid");
        Assert.assertEquals("uid", ak.getUserId());
        ak.setValue("val");
        Assert.assertEquals("val", ak.getValue());
        Date d = new Date();
        ak.setExpirationTime(d);
        Assert.assertEquals(d, ak.getExpirationTime());
    }

}
