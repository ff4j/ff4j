package org.ff4j.web.api.test;

/*
 * #%L
 * ff4j-web
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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

import java.io.IOException;

import org.codehaus.jackson.JsonGenerationException;
import org.codehaus.jackson.map.JsonMappingException;
import org.codehaus.jackson.map.ObjectMapper;
import org.ff4j.web.api.jersey.FF4jJacksonMapper;
import org.ff4j.web.api.resources.domain.FF4jStatusApiBean;
import org.junit.BeforeClass;
import org.junit.Test;

public class JacksonMarshallingTest {
    
    private static ObjectMapper mapper;
    
    @BeforeClass
    public static void init() {
        mapper = new FF4jJacksonMapper().getContext(JacksonMarshallingTest.class);
     }
    
    @Test
    public void testMarshall() throws JsonGenerationException, JsonMappingException, IOException {
        FF4jStatusApiBean fsab = new FF4jStatusApiBean();
        System.out.println(mapper.writeValueAsString(fsab));
    }

}
