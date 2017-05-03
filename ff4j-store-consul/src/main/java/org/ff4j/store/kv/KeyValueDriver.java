package org.ff4j.store.kv;

/**
 * Minimal implementation of key/Store.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public interface KeyValueDriver {
        
    boolean existKey(String key);
    
    void deleteKey(String key);

    void putValue(String key, String value);

    String getValue(String key);
    
    String getFeatureKey(String featureName);
    
    String getFeatureName(String key);
    
    String getPropertyKey(String propertyName);
    
    String getPropertyName(String key);
}
