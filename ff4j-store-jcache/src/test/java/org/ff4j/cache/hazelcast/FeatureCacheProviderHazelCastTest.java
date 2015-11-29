package org.ff4j.cache.hazelcast;

import javax.cache.CacheManager;

import org.ff4j.cache.FF4JCacheManager;
import org.ff4j.test.cache.AbstractCacheManagerJUnitTest;

/**
 * Implementation of {@link CacheManager} for feautres and HazelCast
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FeatureCacheProviderHazelCastTest extends AbstractCacheManagerJUnitTest {

    /** {@inheritDoc} */
    protected FF4JCacheManager getCacheManager() {
        return new FeatureCacheProviderHazelCast();
    }


}
