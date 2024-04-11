package org.ff4j.conf;

/*-
 * #%L
 * ff4j-core
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

import java.util.LinkedHashMap;
import java.util.Map;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.property.Property;

/**
 * Setup FF4j.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class FF4jConfiguration {
    
    /** Core Meta Data. **/ 
    private boolean autoCreate = false;
    
    /** Core Meta Data. **/ 
    private boolean audit = false; 
    
    /** InMemory Features parsed. */
    private Map < String, Feature > features = new LinkedHashMap<>();
    
    /** InMemory Properties parsed. */
    private Map < String, Property<?> > properties = new LinkedHashMap<>();
    
    /** Default constructor. */
    public FF4jConfiguration() {}
    
    /**
     * Used for export.
     *
     * @param ff4j
     */
    public FF4jConfiguration(FF4j ff4j) {
        if (ff4j != null) {
            this.autoCreate = ff4j.isAutocreate();
            this.audit      = ff4j.isEnableAudit();
            
            if (null != ff4j.getFeatureStore()) {
                features = ff4j.getFeatureStore().readAll();
            }
            if (null != ff4j.getPropertiesStore()) {
                properties = ff4j.getPropertiesStore().readAllProperties();
            }
        }
    }

    /**
     * Getter accessor for attribute 'autoCreate'.
     *
     * @return
     *       current value of 'autoCreate'
     */
    public boolean isAutoCreate() {
        return autoCreate;
    }

    /**
     * Setter accessor for attribute 'autoCreate'.
     * @param autoCreate
     * 		new value for 'autoCreate '
     */
    public void setAutoCreate(boolean autoCreate) {
        this.autoCreate = autoCreate;
    }

    /**
     * Getter accessor for attribute 'audit'.
     *
     * @return
     *       current value of 'audit'
     */
    public boolean isAudit() {
        return audit;
    }

    /**
     * Setter accessor for attribute 'audit'.
     * @param audit
     * 		new value for 'audit '
     */
    public void setAudit(boolean audit) {
        this.audit = audit;
    }

    /**
     * Getter accessor for attribute 'features'.
     *
     * @return
     *       current value of 'features'
     */
    public Map<String, Feature> getFeatures() {
        return features;
    }

    /**
     * Setter accessor for attribute 'features'.
     * @param features
     * 		new value for 'features '
     */
    public void setFeatures(Map<String, Feature> features) {
        this.features = features;
    }

    /**
     * Getter accessor for attribute 'properties'.
     *
     * @return
     *       current value of 'properties'
     */
    public Map<String, Property<?>> getProperties() {
        return properties;
    }

    /**
     * Setter accessor for attribute 'properties'.
     * @param properties
     * 		new value for 'properties '
     */
    public void setProperties(Map<String, Property<?>> properties) {
        this.properties = properties;
    }
    
    

}
