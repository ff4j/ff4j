package org.ff4j.cache.it;

import io.lettuce.core.RedisClient;
import org.ff4j.cache.FF4JCacheManager;
import org.ff4j.cache.FF4jCacheManagerRedis;
import org.ff4j.cache.FF4jCacheManagerRedisLettuce;
import org.ff4j.cache.FF4jCacheProxy;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.redis.RedisConnection;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.junit.Ignore;
import org.junit.Test;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import static org.ff4j.test.TestsFf4jConstants.*;


/**
 * Test FF4jCacheManagerRedis caching of properties.
 *
 * @author <a href="https://github.com/mrgrew">Greg Wiley</a>
 *
 * Ignore because Docker may not be available in all cases.
 */
@Ignore
public class PropertyStoreWithRedisCacheTestIT extends RedisTestSupport {

    @Override
    protected FF4JCacheManager makeCache() {
        return new FF4jCacheManagerRedis(new RedisConnection(redis.getHost(), redis.getFirstMappedPort()));
    }

}
