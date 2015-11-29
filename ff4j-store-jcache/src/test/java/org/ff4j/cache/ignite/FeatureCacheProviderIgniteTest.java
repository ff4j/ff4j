package org.ff4j.cache.ignite;

import javax.cache.CacheManager;

import org.ff4j.cache.FF4JCacheManager;
import org.ff4j.test.cache.AbstractCacheManagerJUnitTest;
import org.junit.Ignore;

/**
 * Implementation of {@link CacheManager} for feautres and HazelCast
 *
 * @author Cedrick Lunven (@clunven)</a>
 * 
 * Test are skipped as a grid is required.
 */
@Ignore
public class FeatureCacheProviderIgniteTest extends AbstractCacheManagerJUnitTest {

    /** {@inheritDoc} */
    protected FF4JCacheManager getCacheManager() {
        return new FeatureCacheProviderIgnite();
    }


}
