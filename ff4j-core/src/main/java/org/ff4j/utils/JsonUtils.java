package org.ff4j.utils;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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

import java.util.Map;
import java.util.Set;

import org.ff4j.cache.CacheProxy;
import org.ff4j.property.Property;

/**
 * Custom implementation of serialization : faster + no jackson dependency
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class JsonUtils {
 
    private JsonUtils() {
    }
    
    /**
     * Target primitive displayed as JSON.
     *
     * @param value
     *      object value
     * @return
     *      target json expression
     */
    public static final String valueAsJson(Object value) {
        if (value == null )          return "null";
        if (value instanceof String) return "\"" + value + "\"";
        return value.toString();
    }
    
    public static final String attributeAsJson(String name, Object value) {
        StringBuilder sb = new StringBuilder();
        sb.append(",\"" + name + "\":" + valueAsJson(value));
        return sb.toString();
    }
    
    public static final String objectAsJson(String name, Object value) {
        StringBuilder sb = new StringBuilder();
        sb.append(",\"" + name + "\":" + value);
        return sb.toString();
    }
    
    /**
     * Serialize a collection of object as Json. Element should eventually override <code>toString()</code> to produce JSON.
     *
     * @param pCollec
     *      input collection
     * @return
     *      collection as String
     */
    public static final <T> String collectionAsJson(final Collection < T > pCollec) {
        if (pCollec == null)   return "null";
        if (pCollec.isEmpty()) return "[]";
        StringBuilder json = new StringBuilder("[");
        boolean first = true;
        for (T element : pCollec) {
            json.append(first ? "" : ",");
            json.append(valueAsJson(element));
            first = false;
        }
        json.append("]");
        return json.toString();
    }

    /**
     * Serialize a map of objects as Json. Elements should override <code>toString()</code> to produce JSON.
     *
     * @param customProperties
     *      target properties
     * @return
     *      target json expression
     */
    public static final <K,V> String mapAsJson(final Map<K,V> pMap) {
        if (pMap == null)   return "null";
        if (pMap.isEmpty()) return "{}";
        StringBuilder json = new StringBuilder("{");
        boolean first = true;
        for (Map.Entry<K,V> mapEntry : pMap.entrySet()) {
            json.append(first ? "" : ",");
            json.append(valueAsJson(mapEntry.getKey()) + ":");
            json.append(valueAsJson(mapEntry.getValue()));
            first = false;
        }
        json.append("}");
        return json.toString();
    }
    
    /**
     * Serialize a map of objects as Json. Elements should override <code>toString()</code> to produce JSON.
     *
     * @param customProperties
     *      target properties
     * @return
     *      target json expression
     */
    public static final Map <String, String> jsonAsMap(String jsonString) {
      if (jsonString == null) {
          return null;
      } else if (jsonString.charAt(0) != '{' || jsonString.charAt(jsonString.length()-1) != '}') {
          throw new IllegalArgumentException("Invalid String expected {...}");
      } else if ("{}".equals(jsonString)) {
          return new HashMap<>();
      }
      Map <String, String> response = new HashMap<>();
      // trim { and }
      jsonString = jsonString.substring(1, jsonString.length()-1);
      // Will fail if a string value ends by ','
      Arrays.stream(jsonString.split(",\"")).forEach(chunk -> {
          String[] pair = chunk.split("\":");
          response.put(pair[0].replaceAll("\"", ""), pair[1].replaceAll("\"", ""));
      });
      return response;
    }
    
    /**
     * Cache JSON expression for a store.
     *
     * @param store
     *      current store
     * @return
     *      cache expression
     */
    public static final String cacheJson(Object store) {
        StringBuilder sb = new StringBuilder();
        if (store instanceof CacheProxy<?,?>) {
            CacheProxy<?,?> cacheProxy = (CacheProxy<?,?>) store;
            sb.append(",\"cached\":true");
            sb.append(",\"cacheProvider\":\"" + cacheProxy.getCacheProvider() + "\"");
            sb.append(",\"cacheStore\":\"" + cacheProxy.getTargetStore() + "\"");
        } else {
            sb.append(",\"cached\":false");
        }
        return sb.toString();
    }
    
    /**
     * Generate flipping strategy as json.
     * 
     * @return
     *      flippling strategy as json.     
     */
    public static final String permissionsAsJson(final Set<String> permissions) {
        return collectionAsJson(permissions);
    }
    
    
    /**
     * Serialized custom properties.
     *
     * @param customProperties
     *      target properties
     * @return
     *      target json expression
     */
    public static final String customPropertiesAsJson(final Map<String, ? extends Property<?>> customProperties) {
        return mapAsJson(customProperties);
    }

}
