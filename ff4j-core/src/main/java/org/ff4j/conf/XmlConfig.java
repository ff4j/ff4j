package org.ff4j.conf;

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

import java.util.LinkedHashMap;
import java.util.Map;

import org.ff4j.core.Feature;
import org.ff4j.property.Property;

/**
 * This bean is populated by parsing FF4J XML files.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class XmlConfig {
    
    /** Dedicated Parsing. */
    private Map <String, Feature > features = new LinkedHashMap<String, Feature>();
    
    /** InMemory Feature Map */
    private Map<String, Property<?>> properties = new LinkedHashMap<String, Property<?>>();

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
