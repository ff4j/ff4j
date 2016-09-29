package org.ff4j.archaius;

import org.apache.commons.configuration.AbstractConfiguration;
import org.ff4j.commonsconf.FF4jConfiguration;
import org.ff4j.commonsconf.PropertyStoreCommonsConfig;
import org.ff4j.property.store.PropertyStore;

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

import com.netflix.config.ConfigurationManager;
import com.netflix.config.DynamicConfiguration;

/**
 * As the dynamic configuration from Archaius is compliant with commons-config
 * reuse commons-config operations.
 * 
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyStoreArchaius extends PropertyStoreCommonsConfig {
    
    /**
     * Register Dynamic configuration.
     *
     * @param dc
     *      archiaus {@link DynamicConfiguration} settings
     */
    public PropertyStoreArchaius(AbstractConfiguration dc) {
        super(dc);
        if (!ConfigurationManager.isConfigurationInstalled()) {
            ConfigurationManager.install(dc);
        }
    }
    
    /**
     * Register with source property store.
     *
     * @param source
     *      current property store
     */
    public PropertyStoreArchaius(PropertyStore source) {
        this(new FF4jConfiguration(source));
    }
    
}
