package org.ff4j.store.kv;

/*
 * #%L
 * ff4j-core
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

import java.util.Set;

import org.ff4j.audit.Event;

/**
 * Minimal implementation of key/Store.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public interface KeyValueDriver < K, V > {
        
    boolean existKey(K key);
    
    void deleteKey(K key);

    void putValue(K key, V value);

    V getValue(K key);
    
    // Features
    
    String getFeatureKey(String featureName);
    
    String getFeatureName(K key);
    
    /**
     * Put the reference in the dictionnary, not the feature itself.
     *
     * @param featureName
     *      current feature name
     */
    void registerFeature(String featureName);
    
    void unregisterFeature(String featureName);
    
    Set < String > getFeatureList();
    
    // Properties
    
    String getPropertyKey(String propertyName);
    
    String getPropertyName(K key);
    
    void registerProperty(String propertyName);
    
    void unregisterProperty(String propertyName);
    
    Set < String > getPropertyList();
    
    // Audit
    
    String getHitCountKey(Event e);
    
    String getMissKey(Event e);
    
    String getAuditTrailKey(Event e);
}
