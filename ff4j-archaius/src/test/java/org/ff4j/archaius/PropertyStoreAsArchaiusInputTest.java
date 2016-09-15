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

import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.junit.BeforeClass;
import org.junit.Test;

import com.netflix.config.AbstractPollingScheduler;
import com.netflix.config.ConfigurationManager;
import com.netflix.config.DynamicConfiguration;
import com.netflix.config.DynamicDoubleProperty;
import com.netflix.config.DynamicPropertyFactory;
import com.netflix.config.DynamicStringProperty;
import com.netflix.config.FixedDelayPollingScheduler;
import com.netflix.config.PolledConfigurationSource;

/**
 * Initialization of Archiaus with FF4J as a Source.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyStoreAsArchaiusInputTest {
    
    @BeforeClass
    public static void initArchauisWithFF4j() throws InterruptedException {
        // Sample FF4J Store
        PropertyStore ff4jStore = new InMemoryPropertyStore("ff4j-properties.xml");
        // FF4Store as polling Source for Archiaus
        PolledConfigurationSource ff4jSource = new FF4jPolledConfigurationSource(ff4jStore);
        // Working Thread (polling from ff4j => Archaius)
        AbstractPollingScheduler scheduler = new FixedDelayPollingScheduler(0, 100, true);
        // Define configuration with polling and source
        DynamicConfiguration configuration = new DynamicConfiguration(ff4jSource,scheduler);
        // Init configuration
        ConfigurationManager.install(configuration);
        // Other initialization mechanism
        FF4jPolledConfigurationSource conf2 = new FF4jPolledConfigurationSource();
        conf2.setFf4jStore(ff4jStore);
        // Must invoke poll
        Thread.sleep(100);
    }
    
    @Test
    public void readPropertyInArchaiusFromFF4j() {
        final DynamicStringProperty p1 = DynamicPropertyFactory.getInstance().getStringProperty("e","ko");
        System.out.println(p1.getValue());
        
        final DynamicDoubleProperty d1 = DynamicPropertyFactory.getInstance().getDoubleProperty("c", 13.1);
        System.out.println(d1.getValue());
    }

}
