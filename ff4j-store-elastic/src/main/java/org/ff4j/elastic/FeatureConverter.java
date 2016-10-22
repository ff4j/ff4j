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
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.property.Property;
import org.ff4j.property.util.PropertyFactory;
import org.ff4j.utils.MappingUtil;

import com.google.gson.Gson;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;
import com.google.gson.internal.LinkedTreeMap;
import com.google.gson.reflect.TypeToken;

/**
 * @author <a href="mailto:andre.blaszczyk@gmail.com">Andre Blaszczyk</a>
 *
 *         Custom GSon converter is required to avoid exceptions.
 */
public class FeatureConverter implements JsonSerializer<Feature>, JsonDeserializer<Feature> {

	private Gson gson = new Gson();

	@Override
	public JsonElement serialize(Feature feature, Type srcType, JsonSerializationContext context) {
		return gson.fromJson(feature.toJson(), JsonElement.class);
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	@Override
	public Feature deserialize(JsonElement json, Type type, JsonDeserializationContext context) {

		String uid = json.getAsJsonObject().get("uid").getAsString();

		Feature feature = new Feature(uid);

		// Property "enable"
		JsonElement enable = json.getAsJsonObject().get("enable");
		if (enable != null) {
			feature.setEnable(enable.getAsBoolean());
		}

		// Description
		JsonElement description = json.getAsJsonObject().get("description");
		if (description != null) {
			feature.setDescription(description.getAsString());
		}

		// Group
		JsonElement group = json.getAsJsonObject().get("group");
		if (group != null) {
			feature.setGroup(group.getAsString());
		}

		// Permissions
		JsonElement permissions = json.getAsJsonObject().get("permissions");
		if (permissions != null) {
			Set<String> auths = gson.fromJson(permissions, new TypeToken<HashSet<String>>() {
			}.getType());
			feature.setPermissions(auths);
		}

		// Flipping strategy
		JsonElement flippingStrategy = json.getAsJsonObject().get("flippingStrategy");
		if (flippingStrategy != null && !flippingStrategy.isJsonNull()) {
			Map<String, ?> flippingStrategyParameters = gson.fromJson(flippingStrategy,
					new TypeToken<HashMap<String, ?>>() {
					}.getType());
			String flippingStrategyType = flippingStrategyParameters.get("type").toString();
			Map<String, String> initParams = (Map<String, String>) flippingStrategyParameters.get("initParams");
			// Adding flipping strategy
			feature.setFlippingStrategy(MappingUtil.instanceFlippingStrategy(uid, flippingStrategyType, initParams));
		}

		// Custom properties
		JsonElement customProperties = json.getAsJsonObject().get("customProperties");
		if (customProperties != null && !customProperties.isJsonNull()) {
			Map<String, LinkedTreeMap<String, Object>> map = new Gson().fromJson(customProperties,
					new TypeToken<com.google.gson.internal.LinkedTreeMap<String, LinkedTreeMap<String, Object>>>() {
					}.getType());
			for (Entry<String, LinkedTreeMap<String, Object>> element : map.entrySet()) {
				LinkedTreeMap<String, Object> propertyValues = element.getValue();
				// Getting values
				String pName = (String) propertyValues.get("name");
				String pType = (String) propertyValues.get("type");
				String pValue = (String) propertyValues.get("value");
				String desc = (String) propertyValues.get("description");
				Object fixedValues = propertyValues.get("fixedValues");
				Set pFixedValues = fixedValues != null ? new HashSet((Collection) fixedValues) : null;
				// Creating property with it
				Property<?> property = PropertyFactory.createProperty(pName, pType, pValue, desc, pFixedValues);
				feature.addProperty(property);
			}
		}

		return feature;
	}
}
