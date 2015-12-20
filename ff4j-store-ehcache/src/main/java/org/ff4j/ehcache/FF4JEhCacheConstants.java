package org.ff4j.ehcache;

/*
 * #%L
 * ff4j-store-ehcache
 * %%
 * Copyright (C) 2013 - 2015 FF4J
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


public interface FF4JEhCacheConstants {
    
    /** Default TTL is one hour. */
    long DEFAULT_TIME_TO_LIVE = 120L;

    /** Default time to idle. */
    long DEFAULT_TIME_TO_IDLE = 120L;

    /** Default cache name. */
    String CACHENAME_FEATURES = "ff4jCacheFeatures";
    
    /** Default cache name. */
    String CACHENAME_PROPERTIES = "ff4jCacheProperties";

}
