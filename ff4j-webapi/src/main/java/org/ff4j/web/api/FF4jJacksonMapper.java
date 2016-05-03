package org.ff4j.web.api;

/*
 * #%L
 * ff4j-webapi
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


import javax.ws.rs.ext.ContextResolver;
import javax.ws.rs.ext.Provider;

import org.ff4j.utils.json.FF4jCustomObjectMapper;

import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Customize serializer.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Provider
public class FF4jJacksonMapper extends FF4jCustomObjectMapper implements ContextResolver<ObjectMapper> {
    
    /** {@inheritDoc} */
    public ObjectMapper getContext(Class<?> type) {
        return defaultObjectMapper;
    }
 
    
}

