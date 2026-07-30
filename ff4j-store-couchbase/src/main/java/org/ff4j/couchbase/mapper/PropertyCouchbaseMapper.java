package org.ff4j.couchbase.mapper;

/*-
 * #%L
 * ff4j-store-couchbase
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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


import org.ff4j.exception.FeatureAccessException;
import org.ff4j.mapper.PropertyMapper;
import org.ff4j.property.Property;
import org.ff4j.utils.json.PropertyJsonParser;

import com.couchbase.client.java.json.JsonObject;

/**
 * CRUD operation with couch base API.
 *
 * @author farrellyja
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyCouchbaseMapper implements PropertyMapper<JsonObject> {
    
    /** {@inheritDoc} */
    @Override
    public Property<?> fromStore(JsonObject jsonDoc) {
        if (jsonDoc == null) return null;
        return PropertyJsonParser.parseProperty(jsonDoc.toString());
    }

    /** {@inheritDoc} */
    @Override
    public JsonObject toStore(Property<?> prop)  {
        if (prop == null) return null;
        JsonObject jsonObject;
        try {
            jsonObject = JsonObject.fromJson(prop.toJson());
            jsonObject.put("_class", Property.class.getCanonicalName());
        } catch (Exception e) {
            throw new FeatureAccessException("Cannot parse the feature", e);
        }
        return jsonObject;
    }
  
}
