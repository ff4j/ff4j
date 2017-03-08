package org.ff4j.archaius;

/*
 * #%L
 * ff4j-archaius
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


import java.util.HashMap;
import java.util.Map;

import org.apache.commons.configuration.Configuration;
import org.ff4j.property.Property;
import org.ff4j.property.store.PropertyStore;

import com.netflix.config.PollResult;
import com.netflix.config.PolledConfigurationSource;

/**
 * Bridge from {@link PropertyStore} to commons configuration {@link Configuration}.
 * 
 * Archaius polling InMemoru
 * 
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FF4jPolledConfigurationSource implements PolledConfigurationSource {
    
    /**
     * Source
     */
    private PropertyStore ff4jStore;
    
    /**
     * Default constructor.
     */
    public FF4jPolledConfigurationSource() {
    }
            
    /**
     * Initialized with default value.
     *
     * @param ff4jPropertyStore
     *      default store.
     */
    public FF4jPolledConfigurationSource(PropertyStore ff4jPropertyStore) {
        this.ff4jStore = ff4jPropertyStore;
    }
    
    /** {@inheritDoc} */
    @Override
    public PollResult poll(boolean check, Object arg1) throws Exception {
        if (getFf4jStore() == null) {
            throw new IllegalStateException("PropertyStore should not be null (to poll it !)");
        }
        // Cannot convert to Map < String, Object >
        Map < String, Object > properties = new HashMap<String, Object>();
        for(Map.Entry<String, Property<?>> property : getFf4jStore().readAllProperties().entrySet()) {
            // All properties are String in commons-configuration
            properties.put(property.getKey(), property.getValue().asString());   
        }
        return PollResult.createFull(properties);
    }

    /**
     * Getter accessor for attribute 'ff4jStore'.
     *
     * @return
     *       current value of 'ff4jStore'
     */
    public PropertyStore getFf4jStore() {
        return ff4jStore;
    }

    /**
     * Setter accessor for attribute 'ff4jStore'.
     * @param ff4jStore
     * 		new value for 'ff4jStore '
     */
    public void setFf4jStore(PropertyStore ff4jStore) {
        this.ff4jStore = ff4jStore;
    }
   
}
