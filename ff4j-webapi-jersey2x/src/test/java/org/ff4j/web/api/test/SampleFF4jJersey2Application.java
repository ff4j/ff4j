package org.ff4j.web.api.test;

/*
 * #%L
 * ff4j-webapi-jersey2x
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


import org.ff4j.FF4j;
import org.ff4j.web.ApiConfig;
import org.ff4j.web.api.FF4jApiApplicationJersey2x;

/**
 * Sample application for tests.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class SampleFF4jJersey2Application extends FF4jApiApplicationJersey2x {

    private FF4j ff4j = new FF4j();
    
    public SampleFF4jJersey2Application(FF4j ff) {
        this.ff4j = ff;
    }
    
    @Override
    protected ApiConfig getWebApiConfiguration() {
         ApiConfig fac = new ApiConfig(ff4j);
         fac.setDocumentation(true);
         fac.setPort(3388);
         fac.setHost("localhost");
         fac.setWebContext("webapi");
         return fac;
    }

}