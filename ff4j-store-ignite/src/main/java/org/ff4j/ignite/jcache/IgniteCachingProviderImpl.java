package org.ff4j.ignite.jcache;

/*
 * #%L
 * ff4j-store-ignite
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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
