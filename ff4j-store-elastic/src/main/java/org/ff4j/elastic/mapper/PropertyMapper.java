package org.ff4j.elastic.mapper;

/*-
 * #%L
 * ff4j-store-elastic
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

import java.lang.reflect.Type;
import java.util.HashSet;
import java.util.Set;

import org.ff4j.property.Property;
import org.ff4j.property.util.PropertyFactory;

import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;

/**
 * @author <a href="mailto:andre.blaszczyk@gmail.com">Andre Blaszczyk</a>
 *
 *         Custom GSon converter is required to avoid exceptions.
 */
public class PropertyMapper implements JsonSerializer<Property<?>>, JsonDeserializer<Property<?>> {

    public static final String TYPE_PROPERTY = "property";
    
    /* Attributes in the JSON. */
    public static final String PROPERTY_NAME          = "name";
    public static final String PROPERTY_DESCRIPTION   = "description";
    public static final String PROPERTY_TYPE          = "type";
    public static final String PROPERTY_VALUE         = "value";
    public static final String PROPERTY_FIXED_VALUES  = "fixedValues";
    
    /** Default Gson. */
    private Gson gson = new Gson();

    /** {@inheritDoc} */
	@Override
	public JsonElement serialize(Property<?> property, Type srcType, JsonSerializationContext context) {
		return gson.fromJson(property.toJson(), JsonElement.class);
	}

	/** {@inheritDoc} */
	@SuppressWarnings({ "rawtypes" })
	@Override
	public Property deserialize(JsonElement json, Type type, JsonDeserializationContext context) {
		JsonObject obj = json.getAsJsonObject();
		String pName = obj.get(PROPERTY_NAME) != null ? obj.get(PROPERTY_NAME).getAsString() : null;
		String pDesc = obj.get(PROPERTY_DESCRIPTION) != null ? obj.get(PROPERTY_DESCRIPTION).getAsString() : null;
		String pType = obj.get(PROPERTY_TYPE) != null ? obj.get(PROPERTY_TYPE).getAsString() : null;
		String pValue = obj.get(PROPERTY_VALUE) != null ? obj.get(PROPERTY_VALUE).getAsString() : null;
		JsonArray values = obj.get(PROPERTY_FIXED_VALUES) != null ? obj.get("fixedValues").getAsJsonArray() : null;
		Set<String> pFixedValues = new HashSet<String>();
		if (values != null) {
			for (JsonElement jsonElement : values) {
				pFixedValues.add(jsonElement.getAsString());
			}
		}
		return PropertyFactory.createProperty(pName, pType, pValue, pDesc, pFixedValues);
	}
}
