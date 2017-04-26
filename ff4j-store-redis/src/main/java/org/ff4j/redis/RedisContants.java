package org.ff4j.redis;

/*
 * #%L
 * ff4j-store-redis
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

/**
 * Common constants for Redis implementation.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class RedisContants {

    /** Mapping set for the feature. */
    public static final String KEY_FEATURE_MAP = "FF4J_FEATURE_MAP";

    /** prefix of keys. */
    public static final String KEY_FEATURE = "FF4J_FEATURE_";

    /** Mapping set for properties. */
    public static final String KEY_PROPERTY_MAP = "FF4J_PROPERTY_MAP";

    /** prefix of keys. */
    public static final String KEY_PROPERTY = "FF4J_PROPERTY_";

    /** prefix of keys. */
    public static final String KEY_EVENT = "FF4J_EVENT_";

    /** prefix of keys. */
    public static final String KEY_EVENT_AUDIT = "AUDITRAIL";

    /** default ttl. */
    public static int DEFAULT_TTL = 900000000;

    /**
     * Hide contructor for constants.
     */
    private RedisContants() {
    }

}
