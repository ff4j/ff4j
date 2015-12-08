package org.ff4j.web.api;

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
    
    /**
     * Default Constructor.
     */
    public FF4jJacksonMapper() {
        super();
    }
 
    /** {@inheritDoc} */
    @Override
    public ObjectMapper getContext(Class<?> type) {
        return defaultObjectMapper;
    }
 
    
}

