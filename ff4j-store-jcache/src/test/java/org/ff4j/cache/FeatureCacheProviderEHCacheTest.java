package org.ff4j.cache;

import javax.cache.CacheManager;

import org.ehcache.jsr107.EhcacheCachingProvider;
import org.ff4j.cache.FF4JCacheManager;
import org.ff4j.cache.FF4jJCacheManager;
import org.ff4j.test.cache.AbstractCacheManagerJUnitTest;

/**
 * Implementation of {@link CacheManager} for feautres and HazelCast
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FeatureCacheProviderEHCacheTest extends AbstractCacheManagerJUnitTest {

    /** {@inheritDoc} */
    protected FF4JCacheManager getCacheManager() {
        return new FF4jJCacheManager(EhcacheCachingProvider.class.getName());
    }


}
