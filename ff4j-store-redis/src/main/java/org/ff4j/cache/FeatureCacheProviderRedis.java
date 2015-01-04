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

import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.redis.AbstractRedisProvider;
import org.ff4j.utils.json.FeatureJsonParser;

import redis.clients.jedis.Jedis;

/**
 * Implementation of ditributed cache to limit overhead, with REDIS (JEDIS).
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureCacheProviderRedis extends AbstractRedisProvider implements FeatureCacheManager {
    
    /**
     * Default Constructor.
     */
    public FeatureCacheProviderRedis() {
        jedis = new Jedis(redisHost, redisport);
    }

    /**
     * Contact remote redis server.
     * 
     * @param host
     *            target redis host
     * @param port
     *            target redis port
     */
    public FeatureCacheProviderRedis(String host, int port) {
        jedis = new Jedis(redisHost, redisport);
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        jedis.flushAll();
    }

    /** {@inheritDoc} */
    @Override
    public void evict(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier (param#0) cannot be null nor empty");
        }
        jedis.del(uid);
    }

    /** {@inheritDoc} */
    @Override
    public void put(Feature fp) {
        if (fp == null) {
            throw new IllegalArgumentException("Feature cannot be null nor empty");
        }
        jedis.set(fp.getUid(), fp.toJson());
        jedis.expire(fp.getUid(), timeToLive);
    }

    /** {@inheritDoc} */
    @Override
    public Feature get(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier (param#0) cannot be null nor empty");
        }
        String value = jedis.get(uid);
        if (value != null) {
            return FeatureJsonParser.parseFeature(value);
        }
        return null;
    }
    
    /** {@inheritDoc} */
    @Override
    public Set<String> listCachedFeatureNames() {
        return jedis.keys(PREFIX_KEY + "*");
    }

    /** {@inheritDoc} */
    @Override
    public Object getNativeCache() {
        return jedis;
    }

    /** {@inheritDoc} */
    @Override
    public String getCacheProviderName() {
        return "REDIS";
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
