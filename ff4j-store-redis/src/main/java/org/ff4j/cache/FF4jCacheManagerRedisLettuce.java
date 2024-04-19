package org.ff4j.cache;

/*-
 * #%L
 * ff4j-store-redis
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import io.lettuce.core.*;
import io.lettuce.core.api.StatefulRedisConnection;
import io.lettuce.core.api.sync.RedisKeyCommands;
import io.lettuce.core.api.sync.RedisStringCommands;
import io.lettuce.core.cluster.api.StatefulRedisClusterConnection;
import org.ff4j.core.Feature;
import org.ff4j.property.Property;
import org.ff4j.redis.RedisKeysBuilder;
import org.ff4j.redis.clientsidecache.RedisClientSideCache;
import org.ff4j.redis.clientsidecache.ClientSideCacheRedisKeyCommands;
import org.ff4j.redis.clientsidecache.ClientSideCacheRedisStringCommands;
import org.ff4j.utils.Util;
import org.ff4j.utils.json.FeatureJsonParser;
import org.ff4j.utils.json.PropertyJsonParser;

import io.lettuce.core.cluster.RedisClusterClient;

/**
 * Implementation of distributed cache to limit overhead, with REDIS (Lettuce).
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4jCacheManagerRedisLettuce implements FF4JCacheManager {

    /** time to live for cache on top of store. */
    protected int timeToLive = 900000000;

    /**
     * Holder field for getXXXNativeCache methods.
     * {@link Object} type is unfortunately mandatory here since there is no single common interface
     * for {@link io.lettuce.core.api.sync.RedisCommands} and {@link io.lettuce.core.cluster.api.sync.RedisClusterCommands}.
     */
    private final Object nativeCache;

    /** Access to Redis Key related commands */
    private final RedisKeyCommands<String, String> redisKeyCommands;

    /** Access to Redis String based commands */
    private final RedisStringCommands<String, String> redisStringCommands;

    /** Default key builder. */
    private final RedisKeysBuilder keyBuilder;

    /**
     * Public void.
     */
    public FF4jCacheManagerRedisLettuce(RedisClient redisClient) {
        this(redisClient, new RedisKeysBuilder(), false);
    }

    public FF4jCacheManagerRedisLettuce(RedisClient redisClient, RedisKeysBuilder keyBuilder) {
        this(redisClient, keyBuilder, false);
    }

    public FF4jCacheManagerRedisLettuce(RedisClient redisClient, RedisKeysBuilder keyBuilder, boolean enableClientSideCache) {
        StatefulRedisConnection<String, String> redisConnection = redisClient.connect();
        RedisKeyCommands<String, String> redisKeyCommands = redisConnection.sync();
        RedisStringCommands<String, String> redisStringCommands = redisConnection.sync();
        this.nativeCache = redisConnection.sync();
        this.keyBuilder = keyBuilder;

        if (enableClientSideCache) {
            RedisClientSideCache<String, String> clientSideCache = new RedisClientSideCache<>(redisConnection);
            redisStringCommands = new ClientSideCacheRedisStringCommands<>(redisStringCommands, redisKeyCommands, clientSideCache);
            redisKeyCommands = new ClientSideCacheRedisKeyCommands<>(redisKeyCommands, clientSideCache);
        }

        this.redisKeyCommands = redisKeyCommands;
        this.redisStringCommands = redisStringCommands;
    }

    public FF4jCacheManagerRedisLettuce(RedisClusterClient redisClusterClient) {
        this(redisClusterClient, new RedisKeysBuilder());
    }
    public FF4jCacheManagerRedisLettuce(RedisClusterClient redisClusterClient, RedisKeysBuilder keyBuilder) {
        StatefulRedisClusterConnection<String, String> redisClusterConnection = redisClusterClient.connect();
        this.nativeCache = redisClusterConnection.sync();
        this.redisKeyCommands = redisClusterConnection.sync();
        this.redisStringCommands = redisClusterConnection.sync();
        this.keyBuilder = keyBuilder;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> listCachedFeatureNames() {
        return getKeys(keyBuilder.getKeyFeature("*"));
    }

    /** {@inheritDoc} */
    @Override
    public String getCacheProviderName() {
        return "REDIS";
    }

    /** {@inheritDoc} */
    @Override
    public void clearFeatures() {
        try {
            Set<String> matchingKeys = getKeys(keyBuilder.getKeyFeature("*"));
            if (!matchingKeys.isEmpty()) {
                redisKeyCommands.del(matchingKeys.toArray(new String[matchingKeys.size()]));
            }
        } catch(RuntimeException re) {
            onException(re);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void clearProperties() {
        try {
            Set<String> matchingKeys = getKeys(keyBuilder.getKeyProperty("*"));
            if (!matchingKeys.isEmpty()) {
                redisKeyCommands.del(matchingKeys.toArray(new String[matchingKeys.size()]));
            }
        } catch(RuntimeException re) {
                onException(re);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void evictFeature(String uid) {
        Util.assertParamHasLength(uid, " feature identifier");
        try {
            redisKeyCommands.del(keyBuilder.getKeyFeature(uid));
        } catch(RuntimeException re) {
            onException(re);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void evictProperty(String propertyName) {
        Util.assertParamHasLength(propertyName, " property name");
        try {
            if (propertyName == null || propertyName.isEmpty()) {
                throw new IllegalArgumentException("PropertyName cannot be null nor empty");
            }
            redisKeyCommands.del(keyBuilder.getKeyProperty(propertyName));
        } catch(RuntimeException re) {
            onException(re);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void putFeature(Feature fp) {
        Util.assertNotNull(fp);
        try {
            redisStringCommands.set(keyBuilder.getKeyFeature(fp.getUid()), fp.toJson());
            redisKeyCommands.expire(keyBuilder.getKeyFeature(fp.getUid()), getTimeToLive());
        } catch(RuntimeException re) {
            onException(re);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void putProperty(Property<?> property) {
        Util.assertNotNull(property);
        try {
            redisStringCommands.set(keyBuilder.getKeyProperty(property.getName()), property.toJson());
            redisKeyCommands.expire(keyBuilder.getKeyProperty(property.getName()), getTimeToLive());
        } catch(RuntimeException re) {
            onException(re);
        }
    }

    /** {@inheritDoc} */
    @Override
    public Feature getFeature(String uid) {
        Util.assertParamHasLength(uid, "feature uid");
        String value = "";
        try {
            value = redisStringCommands.get(keyBuilder.getKeyFeature(uid));
            if (value != null) {
                return FeatureJsonParser.parseFeature(value);
            }
        } catch(RuntimeException re) {
            onException(re);
        }
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> getProperty(String propertyName) {
        Util.assertParamHasLength(propertyName, "property name");
        String value = "";
        try {
            value = redisStringCommands.get(keyBuilder.getKeyProperty(propertyName));
            if (value != null) {
                return PropertyJsonParser.parseProperty(value);
            }
        } catch(RuntimeException re) {
            onException(re);
        }
        return null;
    }

    /** {@inheritDoc} */
    public Set<String> listCachedPropertyNames() {
        List<String> keys = new ArrayList<>();
        try {
            keys = redisKeyCommands.keys(keyBuilder.getKeyProperty("*"));
        } catch (RuntimeException re) {
            onException(re);
        }
        return new HashSet<>(keys);
    }

    /** {@inheritDoc} */
    public Object getFeatureNativeCache() {
        return nativeCache;
    }

    /** {@inheritDoc} */
    public Object getPropertyNativeCache() {
        return nativeCache;
    }

    private Set<String> getKeys(String pattern) {
        try {
            KeyScanCursor<String> ksc = redisKeyCommands.scan(new ScanArgs().match(pattern));
            Set<String> matchingKeys = new HashSet<>(ksc.getKeys());
            while (!ksc.isFinished()) {
                ksc = redisKeyCommands.scan(ksc);
                matchingKeys.addAll(ksc.getKeys());
            }
            return matchingKeys;
        } catch (RuntimeException re) {
            onException(re);
        }
        return null;
    }

    /**
     * Getter accessor for attribute 'timeToLive'.
     *
     * @return
     *       current value of 'timeToLive'
     */
    public int getTimeToLive() {
        return timeToLive;
    }

    /**
     * Setter accessor for attribute 'timeToLive'.
     *
     * @param timeToLive
     *      new value for 'timeToLive '
     */
    public void setTimeToLive(int timeToLive) {
        this.timeToLive = timeToLive;
    }

    /**
     * Getter accessor for attribute 'keyBuilder'.
     *
     * @return
     *       current value of 'keyBuilder'
     */
    public RedisKeysBuilder getKeyBuilder() {
        return keyBuilder;
    }

}
