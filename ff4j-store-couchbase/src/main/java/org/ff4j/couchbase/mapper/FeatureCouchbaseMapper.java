package org.ff4j.couchbase.mapper;

import org.ff4j.core.Feature;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.mapper.FeatureMapper;
import org.ff4j.utils.json.FeatureJsonParser;

import com.couchbase.client.java.document.JsonDocument;
import com.couchbase.client.java.document.json.JsonObject;
import com.couchbase.client.java.transcoder.JsonTranscoder;

/**
 * CRUD operation with couch base API.
 *
 * @author farrellyja
 * @author Cedrick LUNVEN (@clunven)
 */
public class FeatureCouchbaseMapper implements FeatureMapper< JsonDocument >{
    
    /** Help JSON conversion. */
    private static final JsonTranscoder TRANSCODER = new JsonTranscoder();
    
    /** {@inheritDoc} */
    @Override
    public Feature fromStore(JsonDocument jsonDoc) {
        if (jsonDoc == null) return null;
        return FeatureJsonParser.parseFeature(jsonDoc.content().toString());
    }

    /** {@inheritDoc} */
    @Override
    public JsonDocument toStore(Feature feature)  {
        if (feature == null) return null;
        JsonObject jsonObject;
        try {
            jsonObject = TRANSCODER.stringToJsonObject(feature.toJson());
            jsonObject.put("_class", Feature.class.getCanonicalName());
        } catch (Exception e) {
            throw new FeatureAccessException("Cannot parse the feature", e);
        }
        return JsonDocument.create(feature.getUid(), jsonObject);
    }
  
}
