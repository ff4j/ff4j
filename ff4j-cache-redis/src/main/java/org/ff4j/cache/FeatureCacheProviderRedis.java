package org.ff4j.cache;

/*
 * #%L
 * ff4j-cache-redis
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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

import org.ff4j.core.Feature;
import org.ff4j.utils.FeatureJsonMarshaller;

import redis.clients.jedis.Jedis;

public class FeatureCacheProviderRedis implements FeatureCacheManager {

    /** default host. */
    public static final String DEFAULT_REDIS_HOST = "localhost";

    /** default port. */
    public static final int DEFAULT_REDIS_PORT = 6379;

    /** redis host. */
    private final String redisHost = DEFAULT_REDIS_HOST;

    /** redis port. */
    private final int redisport = DEFAULT_REDIS_PORT;

    /** time to live. */
    private final int timeToLive = Integer.MAX_VALUE;

    /** Java Redis CLIENT. */
    private final Jedis jedis;

    public FeatureCacheProviderRedis() {
        jedis = new Jedis(redisHost, redisport);
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        jedis.flushAll();
    }

    /** {@inheritDoc} */
    @Override
    public void evict(String featureId) {
        jedis.del(featureId);
    }

    /** {@inheritDoc} */
    @Override
    public void put(Feature feat) {
        jedis.set(feat.getUid(), feat.toString());
        jedis.expire(feat.getUid(), timeToLive);
    }

    /** {@inheritDoc} */
    @Override
    public Feature get(String featureId) {
        return FeatureJsonMarshaller.unMarshallFeature(jedis.get(featureId));
    }

    /** {@inheritDoc} */
    @Override
    public Object getNativeCache() {
        return jedis;
    }

    /**
     * Getter accessor for attribute 'redisHost'.
     * 
     * @return current value of 'redisHost'
     */
    public String getRedisHost() {
        return redisHost;
    }

    /**
     * Getter accessor for attribute 'redisport'.
     * 
     * @return current value of 'redisport'
     */
    public int getRedisport() {
        return redisport;
    }

    /**
     * Getter accessor for attribute 'timeToLive'.
     * 
     * @return current value of 'timeToLive'
     */
    public int getTimeToLive() {
        return timeToLive;
    }

}
