package org.ff4j.ehcache;

import java.io.InputStream;

import net.sf.ehcache.Cache;
import net.sf.ehcache.CacheManager;
import net.sf.ehcache.config.Configuration;

/**
 * Wrapping EHCache Cache to mutualized initialization.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public final class FF4jEhCacheWrapper {
   
    /** Default cache name. */
    public static final String CACHENAME_FEATURES = "ff4jCacheFeatures";
    
    /** Default cache name. */
    public static final String CACHENAME_PROPERTIES = "ff4jCacheProperties";
    
    /** The cache manager. */
    private CacheManager cacheManager;
    
    /** Setup Cache configuration to work with Terracotta for instance. */
    private Configuration cacheConfiguration;
    
    /** Configuration file. */
    private InputStream cacheConfigurationFile;
   
    /** Eh Cache - cache-aside mode utlization. */
    private Cache cacheFeatures = null;
    
    /** Eh Cache - cache-aside mode utlization. */
    private Cache cacheProperties = null;
    
    /**
     * Default Constructor.
     */
    public FF4jEhCacheWrapper() {
    }
    
    /**
     * Default Constructor.
     */
    public FF4jEhCacheWrapper(Configuration cacheConfig) {
        this.cacheConfiguration = cacheConfig;
    }
    
    /**
     * Default Constructor.
     */
    public FF4jEhCacheWrapper(String xmlEhCacheConfig) {
        this.cacheConfigurationFile = getClass().getResourceAsStream(xmlEhCacheConfig);
        if (cacheConfigurationFile == null) {
            throw new IllegalArgumentException("Cannot find resource '" + xmlEhCacheConfig + "' in classpath, please check path");
        }
    }
    
    private CacheManager getCacheManager() {
        if (cacheManager == null) {
            if (null != cacheConfiguration) {
                this.cacheManager = CacheManager.create(cacheConfiguration);
                
            } else if (cacheConfigurationFile != null){
                this.cacheManager = CacheManager.create(cacheConfigurationFile);
            } else {
                // expect to find the ehcache.xml file in classpath
                this.cacheManager = CacheManager.create();
            }
        }
        return cacheManager;
    }
    
    public Cache getCacheProperties() {
        if (cacheProperties == null) {
            if (!getCacheManager().cacheExists(CACHENAME_PROPERTIES)) {
                getCacheManager().addCache(CACHENAME_PROPERTIES);
            }
            cacheProperties = getCacheManager().getCache(CACHENAME_PROPERTIES);
        }
        return cacheProperties;
    }
        
    public Cache getCacheFeatures() {
        if (cacheFeatures == null) {
            if (!getCacheManager().cacheExists(CACHENAME_FEATURES)) {
                getCacheManager().addCache(CACHENAME_FEATURES);
            }
            cacheFeatures   = getCacheManager().getCache(CACHENAME_FEATURES);
        }
        return cacheFeatures;
    }

}
