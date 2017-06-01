package org.ff4j.ignite;

import javax.cache.CacheManager;
import javax.cache.spi.CachingProvider;

import org.apache.ignite.Ignite;
import org.apache.ignite.Ignition;
import org.apache.ignite.configuration.IgniteConfiguration;
import org.ff4j.cache.FF4jJCacheManager;
import org.ff4j.ignite.jcache.IgniteCachingProviderImpl;

/**
 * Implementation of {@link CacheManager} for feautres and HazelCast
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FF4jCacheManagerIgnite extends FF4jJCacheManager {
   
    /** Internal HazelCast Settings. */
    private Ignite ignite = null;
    
    /**
     * Initialization of HazelCast with default config.
     *
     * @param hazelCastConfig
     */
    public FF4jCacheManagerIgnite() {
        this(Ignition.start());
    }
      
    /**
     * Initialization of HazelCast with config fileName (and properties)
     *
     * @param hazelCastConfig
     */
    public FF4jCacheManagerIgnite(String xmlConfigFileName) {
        this(Ignition.start(xmlConfigFileName));
    }
    
    /**
     * Initialization of HazelCast with config fileName (and properties)
     *
     * @param hazelCastConfig
     */
    public FF4jCacheManagerIgnite(IgniteConfiguration cfg) {
        this(Ignition.start(cfg));
    }
    
    /**
     * Initialization of HazelCast with provided config (and properties)
     *
     * @param hazelCastConfig
     */
    public FF4jCacheManagerIgnite(Ignite pIgnite) {
        this.ignite = pIgnite;
        initCachingProvider(null);
    }
    
    /** {@inheritDoc} */
    @Override
    public CachingProvider initCachingProvider(String className) {
        if (getIgnite() == null) {
            throw new IllegalStateException("Cannot initialize cache, no configuration found");
        }
        setCachingProvider(new IgniteCachingProviderImpl(getIgnite()));
        return getCachingProvider();
    }
    
    /**
     * Getter accessor for attribute 'ignite'.
     *
     * @return
     *       current value of 'ignite'
     */
    public Ignite getIgnite() {
        return ignite;
    }

    /**
     * Setter accessor for attribute 'ignite'.
     * @param ignite
     * 		new value for 'ignite '
     */
    public void setIgnite(Ignite ignite) {
        this.ignite = ignite;
    }

}
