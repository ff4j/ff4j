package org.ff4j.utils.json;

import java.text.SimpleDateFormat;

import org.codehaus.jackson.map.DeserializationConfig;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.map.SerializationConfig;
import org.codehaus.jackson.map.annotate.JsonSerialize;
import org.codehaus.jackson.map.introspect.JacksonAnnotationIntrospector;

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
        mapper.setSerializationInclusion(JsonSerialize.Inclusion.NON_NULL);
        mapper.setSerializationInclusion(JsonSerialize.Inclusion.NON_EMPTY);
        mapper.getSerializationConfig().without(SerializationConfig.Feature.FAIL_ON_EMPTY_BEANS);
        mapper.getDeserializationConfig().without(DeserializationConfig.Feature.FAIL_ON_NULL_FOR_PRIMITIVES);
        mapper.getDeserializationConfig().without(DeserializationConfig.Feature.FAIL_ON_UNKNOWN_PROPERTIES);
        mapper.setDateFormat(new SimpleDateFormat("dd/MM/yyyy"));
        return mapper;
    }

}
