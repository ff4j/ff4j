package org.ff4j.ignite.jcache;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Properties;

import javax.cache.CacheManager;
import javax.cache.configuration.OptionalFeature;
import javax.cache.spi.CachingProvider;

import org.apache.ignite.Ignite;

/**
 * Implementation of {@link CachingProvider} for Ignite as not provided (!)
 * 
 * @author Cedrick LUNVEN (@clunven)
 */
public class IgniteCachingProviderImpl implements CachingProvider {

    /** Internal HazelCast Settings. */
    private Ignite ignite = null;
    
    /** Relative cache manager. */
    private CacheManager cacheManager = null;
    
    /**
     * Default constructor.
     * 
     * @param ini
     *      ignite configuration bean
     */
    public IgniteCachingProviderImpl(Ignite ini) {
        this.ignite       = ini;
        this.cacheManager = new IgniteCachingManagerImpl(this);
    }
    
    /** {@inheritDoc} */
    @Override
    public ClassLoader getDefaultClassLoader() {
        return getClass().getClassLoader();
    }
    
    /** {@inheritDoc} */
    @Override
    public CacheManager getCacheManager(URI uri, ClassLoader classLoader, Properties properties) {
        return getCacheManager();
    }

    /** {@inheritDoc} */
    @Override
    public URI getDefaultURI() {
        try {
            return new URI(ignite.configuration().getConnectorConfiguration().getHost());
        } catch (URISyntaxException e) {
            return null;
        }
    }

    /** {@inheritDoc} */
    @Override
    public Properties getDefaultProperties() {
        return new Properties();
    }
    
    /** {@inheritDoc} */
    @Override
    public CacheManager getCacheManager(URI uri, ClassLoader classLoader) {
        return getCacheManager();
    }

    /** {@inheritDoc} */
    @Override
    public CacheManager getCacheManager() {
        return cacheManager;
    }

    /** {@inheritDoc} */
    @Override
    public void close() {
        ignite.close();
    }

    /** {@inheritDoc} */
    @Override
    public void close(ClassLoader classLoader) {
        this.close();
    }

    /** {@inheritDoc} */
    @Override
    public void close(URI uri, ClassLoader classLoader) {
        this.close();
    }

    /** {@inheritDoc} */
    @Override
    public boolean isSupported(OptionalFeature optionalFeature) {
        return false;
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

}
