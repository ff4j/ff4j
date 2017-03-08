package org.ff4j.commonsconf;

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


import org.apache.commons.configuration.CompositeConfiguration;
import org.apache.commons.configuration.ConfigurationException;
import org.ff4j.property.store.InMemoryPropertyStore;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * FF4JConfiguration could be used to inject properties into commons config.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class CommonsConfigWithPropertyStoreTest {
    
    /** Configuration from Commons-conf. */
    private CompositeConfiguration config;
    
    /** Configuration implementation relying on property store. */
    private FF4jConfiguration ff4jConf;
            
    @Before
    public void initCommonsConfWithFF4j() throws ConfigurationException {
        // init configuration
        ff4jConf = new FF4jConfiguration(new InMemoryPropertyStore("ff4j-properties.xml"));
        // composite
        config = new CompositeConfiguration();
        config.setThrowExceptionOnMissing(true);
        config.addConfiguration(ff4jConf);
    }
    
    @Test
    public void readPropertyInCommonsConfFromFF4j() throws ConfigurationException {
        
        // Retrieve data from FF4J
        Assert.assertEquals("hello", config.getString("e"));
        
        // Retrieve whole property if required
        Assert.assertEquals("comment", ff4jConf.getFf4jStore().readProperty("e").getDescription());
    }

}
