package org.ff4j.utils;

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
import java.util.Map.Entry;

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
        StringBuilder json = new StringBuilder();
        if (null != permissions) {
            json.append("[");
            if (!permissions.isEmpty()) {
                boolean first = true;
                for (String auth : permissions) {
                    json.append(first ? "" : ",");
                    json.append("\"" + auth + "\"");
                    first = false;
                }
            }
            json.append("]");
        } else {
            json.append("null");
        }
        return json.toString();
    }
    
    /**
     * Generate flipping strategy as json.
     * 
     * @return
     *      flippling strategy as json.     
     */
    public static final String flippingStrategyAsJson(final FlippingStrategy flippingStrategy) {
        StringBuilder json = new StringBuilder();
        if (null != flippingStrategy) {
            json.append("{\"initParams\":{");
            Map < String , String> iparams = flippingStrategy.getInitParams();
            if (iparams != null && !iparams.isEmpty()) {
                boolean first = true;
                for (Entry<String, String> param : iparams.entrySet()) {
                    json.append(first ? "" : ",");
                    json.append("\"" + param.getKey() + "\":\"" + param.getValue() + "\"");
                    first = false;
                }
            }
            json.append("},\"type\":\"");
            json.append(flippingStrategy.getClass().getCanonicalName());
            json.append("\"}");
        } else {
            json.append("null");
        }
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
        StringBuilder json = new StringBuilder("{");
        if (null != customProperties && !customProperties.isEmpty()) {
            boolean first = true;
            for (Map.Entry<String, ? extends Property<?>> key : customProperties.entrySet()) {
                json.append(first ? "" : ",");
                json.append("\"" + key.getKey() + "\":" + key.getValue().toJson());
                first = false;
            }
        }
        json.append("}");
        return json.toString();
    }

}
