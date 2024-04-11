package org.ff4j.hazelcast;

/*-
 * #%L
 * ff4j-store-hazelcast
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


import java.util.Properties;

import javax.cache.CacheManager;
import javax.cache.Caching;
import javax.cache.spi.CachingProvider;

import org.ff4j.cache.FF4jJCacheManager;

import com.hazelcast.cache.HazelcastMemberCachingProvider;
import com.hazelcast.config.ClasspathXmlConfig;
import com.hazelcast.config.Config;
import com.hazelcast.config.XmlConfigBuilder;

/**
 * Implementation of {@link CacheManager} for feautres and HazelCast
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class CacheManagerHazelCast extends FF4jJCacheManager {
   
    /** Specialize Hazelcast configuration. */
    private Config hazelCastConfig = null;
    
    /**
     * Initialization of HazelCast with default config.
     *
     * @param hazelCastConfig
     */
    public CacheManagerHazelCast() {
        this(new XmlConfigBuilder().build(), null);
    }
      
    /**
     * Initialization of HazelCast with config fileName (and properties)
     *
     * @param hazelCastConfig
     */
    public CacheManagerHazelCast(String xmlConfigFileName, Properties systemProperties) {
        this(new ClasspathXmlConfig(xmlConfigFileName), systemProperties);
    }
    
    /**
     * Initialization of HazelCast with provided config (and properties)
     *
     * @param hazelCastConfig
     */
    public CacheManagerHazelCast(Config hazelCastConfig, Properties systemProperties) {
        if (systemProperties != null) {
            System.setProperties(systemProperties);
        }
        this.hazelCastConfig = hazelCastConfig;
        initCachingProvider(null);
    }
    
    /** {@inheritDoc} */
    @Override
    public CachingProvider initCachingProvider(String className) {
        // Deprecated in 5.1
        //if (getHazelCastConfig() == null) throw new IllegalStateException("Cannot initialize cache, no configuration found");
        //HazelcastInstance instance = Hazelcast.newHazelcastInstance(getHazelCastConfig());
        //setCachingProvider(HazelcastServerCachingProvider.createCachingProvider(instance));
        CachingProvider provider = Caching.getCachingProvider(HazelcastMemberCachingProvider.class.getName());
        setCachingProvider(provider);
        return getCachingProvider();
    }
    
    /**
     * Getter accessor for attribute 'hazelCastConfig'.
     *
     * @return
     *       current value of 'hazelCastConfig'
     */
    public Config getHazelCastConfig() {
        return hazelCastConfig;
    }

    /**
     * Setter accessor for attribute 'hazelCastConfig'.
     * @param hazelCastConfig
     * 		new value for 'hazelCastConfig '
     */
    public void setHazelCastConfig(Config hazelCastConfig) {
        this.hazelCastConfig = hazelCastConfig;
    }

}
