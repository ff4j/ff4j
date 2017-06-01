package org.ff4j.hazelcast;

import java.util.Properties;

import javax.cache.CacheManager;
import javax.cache.spi.CachingProvider;

import org.ff4j.cache.FF4jJCacheManager;

import com.hazelcast.cache.impl.HazelcastServerCachingProvider;
import com.hazelcast.config.ClasspathXmlConfig;
import com.hazelcast.config.Config;
import com.hazelcast.config.XmlConfigBuilder;
import com.hazelcast.core.Hazelcast;
import com.hazelcast.core.HazelcastInstance;

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
        if (getHazelCastConfig() == null) throw new IllegalStateException("Cannot initialize cache, no configuration found");
        HazelcastInstance instance = Hazelcast.newHazelcastInstance(getHazelCastConfig());
        setCachingProvider(HazelcastServerCachingProvider.createCachingProvider(instance));
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
