package org.ff4j.consul;

/*
 * #%L
 * ff4j-store-consul
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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
 * Set of keys to register ff4j into Consul registry.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class ConsulConstants {

    /** Global Key. */
    public static final String FF4J_KEY_FF4J = "FF4J";
    
    /** Path for ff4j keys. */
    public static final String FF4J_PREFIXKEY_FEATURES = FF4J_KEY_FF4J + "/FEATURES/";
    
    /** Path for ff4j keys. */
    public static final String FF4J_PREFIXKEY_PROPERTIES = FF4J_KEY_FF4J + "/PROPERTIES/";
    
    /** Path for ff4j keys. */
    public static final String FF4J_PREFIXKEY_AUDIT = FF4J_KEY_FF4J + "/AUDIT/";
    
    /** Path for ff4j keys. */
    public static final String FF4J_PREFIXKEY_HITS = FF4J_KEY_FF4J + "/HITS/";
    
    /** Path for ff4j keys. */
    public static final String FF4J_PREFIXKEY_MISS = FF4J_KEY_FF4J + "/MISS/";
}
