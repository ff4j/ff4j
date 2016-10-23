package org.ff4j.elastic;

/*
 * #%L
 * ff4j-store-elastic
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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
public class PropertyConverter implements JsonSerializer<Property<?>>, JsonDeserializer<Property<?>> {

	private Gson gson = new Gson();

	@Override
	public JsonElement serialize(Property<?> property, Type srcType, JsonSerializationContext context) {
		return gson.fromJson(property.toJson(), JsonElement.class);
	}

	@SuppressWarnings({ "rawtypes" })
	@Override
	public Property deserialize(JsonElement json, Type type, JsonDeserializationContext context) {

		JsonObject obj = json.getAsJsonObject();

		// Getting attributes
		String pName = obj.get("name") != null ? obj.get("name").getAsString() : null;
		String pDesc = obj.get("description") != null ? obj.get("description").getAsString() : null;
		String pType = obj.get("type") != null ? obj.get("type").getAsString() : null;
		String pValue = obj.get("value") != null ? obj.get("value").getAsString() : null;
		JsonArray values = obj.get("fixedValues") != null ? obj.get("fixedValues").getAsJsonArray() : null;
		Set<String> pFixedValues = new HashSet<String>();
		if (values != null) {
			for (JsonElement jsonElement : values) {
				pFixedValues.add(jsonElement.getAsString());
			}
		}

		return PropertyFactory.createProperty(pName, pType, pValue, pDesc, pFixedValues);
	}
}
