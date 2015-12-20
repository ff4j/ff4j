package org.ff4j.utils.json;

/*
 * #%L
 * ff4j-utils-json
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


import java.text.SimpleDateFormat;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import com.fasterxml.jackson.databind.introspect.JacksonAnnotationIntrospector;

public class FF4jCustomObjectMapper {
    
    /** Jackson Mapper. */
    protected final ObjectMapper defaultObjectMapper;
    
    /**
     * Default Constructor.
     */
    public FF4jCustomObjectMapper() {
        defaultObjectMapper = createDefaultMapper();
    }
 
    /**
     * Custom ObjectMapper
     * @return
     *      target object mapper
     */
    private static ObjectMapper createDefaultMapper() {
        final ObjectMapper mapper = new ObjectMapper();
        mapper.setAnnotationIntrospector(new JacksonAnnotationIntrospector());
        mapper.configure(JsonParser.Feature.ALLOW_SINGLE_QUOTES, true);
        mapper.configure(JsonParser.Feature.ALLOW_UNQUOTED_FIELD_NAMES, true);
        mapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
        mapper.configure(DeserializationFeature.FAIL_ON_NULL_FOR_PRIMITIVES, false);
        mapper.configure(SerializationFeature.FAIL_ON_EMPTY_BEANS, false);
        mapper.configure(JsonGenerator.Feature.ESCAPE_NON_ASCII, true);
        mapper.setDateFormat(new SimpleDateFormat("dd/MM/yyyy"));
        return mapper;
    }

}
