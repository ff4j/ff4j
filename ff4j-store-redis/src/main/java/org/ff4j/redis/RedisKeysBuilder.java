package org.ff4j.redis;

/*-
 * #%L
 * ff4j-store-redis
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

import java.text.SimpleDateFormat;
import java.util.Date;

/**
 * Create keys in a single place to allow extensions, prefix, suffix.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class RedisKeysBuilder {
    
    /** Pattern to create KEY. */
    private static final SimpleDateFormat SDF_KEY = new SimpleDateFormat("yyyyMMdd");
    
    /** Key constants */
    private static final String DEFAULT_KEY_PREFIX = "FF4J_";
    private static final String DEFAULT_KEY_SUFFIX = "";

    /** prefix of keys. */
    public static final String KEY_EVENT = "FF4J_EVENT_AUDITRAIL_";
    
    /** key prefix. */
    public String keyPrefix = DEFAULT_KEY_PREFIX;
    
    /** key suffix. */
    public String keySuffix = DEFAULT_KEY_SUFFIX;
    
    /** 
     * Default constructor. 
     **/
    public RedisKeysBuilder() {
        this(DEFAULT_KEY_PREFIX, DEFAULT_KEY_SUFFIX);
    }
    
    /** 
     * Default constructor. 
     **/
    public RedisKeysBuilder(String prefix) {
        this(prefix, DEFAULT_KEY_SUFFIX);
    }
    
    /** 
     * Default constructor. 
     **/
    public RedisKeysBuilder(String prefix, String suffix) {
        if (null != prefix) {
            this.keyPrefix = prefix;
        }
        if (null != suffix) {
            this.keySuffix = suffix;
        }
    }
    
    /**
     * Getter accessor for attribute 'keyPrefix'.
     *
     * @return
     *       current value of 'keyPrefix'
     */
    public String getKeyPrefix() {
        return keyPrefix;
    }

    /**
     * Getter accessor for attribute 'keySuffix'.
     *
     * @return
     *       current value of 'keySuffix'
     */
    public String getKeySuffix() {
        return keySuffix;
    }
    
    public String getKeyFeatureMap() {
        return getKeyPrefix() + "FEATURE_MAP" + getKeySuffix();
    }
        
    public String getKeyFeature(String id) {
        return getKeyPrefix() + "FEATURE_" + id + getKeySuffix() ;
    }
    
    public String getKeyPropertyMap() {
        return getKeyPrefix() + "PROPERTY_MAP" + getKeySuffix();
    }
    
    public String getKeyProperty(String name) {
        return getKeyPrefix() + "PROPERTY_" + name + getKeySuffix();
    }
    
    public String getKeyEvent() {
        return getKeyPrefix() + "EVENT_AUDITRAIL_" + getKeySuffix();
    }
    
    public String getHashKey(long timestamp) {
        return getKeyEvent() + SDF_KEY.format(new Date(timestamp));
    }

     
    
    
}
