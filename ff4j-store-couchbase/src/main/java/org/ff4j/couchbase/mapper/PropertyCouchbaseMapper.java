package org.ff4j.couchbase.mapper;

import org.ff4j.exception.FeatureAccessException;
import org.ff4j.mapper.PropertyMapper;
import org.ff4j.property.Property;
import org.ff4j.utils.json.PropertyJsonParser;

import com.couchbase.client.java.document.JsonDocument;
import com.couchbase.client.java.document.json.JsonObject;
import com.couchbase.client.java.transcoder.JsonTranscoder;

/**
 * CRUD operation with couch base API.
 *
 * @author farrellyja
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyCouchbaseMapper implements PropertyMapper< JsonDocument > {
    
    /** Help JSON conversion. */
    private static final JsonTranscoder TRANSCODER = new JsonTranscoder();
    
    /** {@inheritDoc} */
    @Override
    public Property<?> fromStore(JsonDocument jsonDoc) {
        if (jsonDoc == null) return null;
        return PropertyJsonParser.parseProperty(jsonDoc.content().toString());
    }

    /** {@inheritDoc} */
    @Override
    public JsonDocument toStore(Property<?> prop)  {
        if (prop == null) return null;
        JsonObject jsonObject;
        try {
            jsonObject = TRANSCODER.stringToJsonObject(prop.toJson());
            jsonObject.put("_class", Property.class.getCanonicalName());
        } catch (Exception e) {
            throw new FeatureAccessException("Cannot parse the feature", e);
        }
        return JsonDocument.create(prop.getName(), jsonObject);
    }
  
}
