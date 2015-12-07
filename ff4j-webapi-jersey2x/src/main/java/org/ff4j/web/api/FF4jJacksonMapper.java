package org.ff4j.web.api;

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

import java.text.SimpleDateFormat;

import javax.ws.rs.ext.ContextResolver;
import javax.ws.rs.ext.Provider;

import org.codehaus.jackson.map.DeserializationConfig;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.SerializationConfig;
import org.codehaus.jackson.map.annotate.JsonSerialize;
import org.codehaus.jackson.map.introspect.JacksonAnnotationIntrospector;

/**
 * Customize serializer.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Provider
public class FF4jJacksonMapper implements ContextResolver<ObjectMapper> {

    /** Jackson Mapper. */
    private final ObjectMapper defaultObjectMapper;
    
    /**
     * Default Constructor.
     */
    public FF4jJacksonMapper() {
        defaultObjectMapper = createDefaultMapper();
    }
 
    /** {@inheritDoc} */
    @Override
    public ObjectMapper getContext(Class<?> type) {
        return defaultObjectMapper;
    }
 
    /**
     * Custom ObjectMapper
     * @return
     *      target object mapper
     */
    private static ObjectMapper createDefaultMapper() {
        final ObjectMapper mapper = new ObjectMapper();
        mapper.setAnnotationIntrospector(new JacksonAnnotationIntrospector());
        mapper.setSerializationInclusion(JsonSerialize.Inclusion.NON_NULL);
        mapper.setSerializationInclusion(JsonSerialize.Inclusion.NON_EMPTY);
        mapper.getSerializationConfig().without(SerializationConfig.Feature.FAIL_ON_EMPTY_BEANS);
        mapper.getDeserializationConfig().without(DeserializationConfig.Feature.FAIL_ON_NULL_FOR_PRIMITIVES);
        mapper.getDeserializationConfig().without(DeserializationConfig.Feature.FAIL_ON_UNKNOWN_PROPERTIES);
        mapper.setDateFormat(new SimpleDateFormat("dd/MM/yyyy"));
        return mapper;
    }
 
    
}

