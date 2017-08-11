package org.ff4j.utils;

import java.util.Collection;

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

import org.ff4j.cache.FF4jCacheProxy;
import org.ff4j.core.FlippingStrategy;
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
     * Cache JSON expression for a store.
     *
     * @param store
     *      current store
     * @return
     *      cache expression
     */
    public static final String cacheJson(Object store) {
        StringBuilder sb = new StringBuilder();
        if (store instanceof FF4jCacheProxy) {
            FF4jCacheProxy cacheProxy = (FF4jCacheProxy) store;
            sb.append(",\"cached\":true");
            sb.append(",\"cacheProvider\":\"" + cacheProxy.getCacheProvider() + "\"");
            sb.append(",\"cacheStore\":\"" + cacheProxy.getCachedTargetStore() + "\"");
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
     * Generate flipping strategy as json.
     * 
     * @return
     *      flippling strategy as json.     
     */
    public static final String flippingStrategyAsJson(final FlippingStrategy flippingStrategy) {
        if (flippingStrategy == null) return "null";
        StringBuilder json = new StringBuilder("{");
        json.append(valueAsJson("initParams") + ":");
        json.append(mapAsJson(flippingStrategy.getInitParams()));
        json.append("," + valueAsJson("type")  + ":");
        json.append(valueAsJson(flippingStrategy.getClass().getName()));
        json.append("}");
        return json.toString();
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
