package org.ff4j.web.store;

/*
 * #%L
 * ff4j-webapi-jersey2x
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.web.jersey2.store.FeatureStoreHttp;
import org.ff4j.web.jersey2.store.PropertyStoreHttp;
import org.junit.Test;

public class FeatureStoreHttpSecured {
    
    @Test
    public void testReadFeature() {
        String targetRestApiURL = "http://localhost:8080/api/ff4j/";
        String userName         = "user";
        String userPassword     = "userPass";
        
        
        // Init FF4j as HTTP CLIENT
        FF4j ff4j = new  FF4j();
        ff4j.setFeatureStore(new FeatureStoreHttp(targetRestApiURL, userName, userPassword));
        ff4j.setPropertiesStore(new PropertyStoreHttp(targetRestApiURL, userName, userPassword));
        
        Feature f1 = ff4j.getFeatureStore().read("f1");
        System.out.println(f1);
        
        
    }

}
