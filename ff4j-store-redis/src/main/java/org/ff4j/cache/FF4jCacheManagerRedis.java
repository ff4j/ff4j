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

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.ff4j.core.Feature;
import org.ff4j.property.Property;
import org.ff4j.redis.RedisConnection;
import org.ff4j.redis.RedisKeysBuilder;
import org.ff4j.utils.Util;
import org.ff4j.utils.json.FeatureJsonParser;
import org.ff4j.utils.json.PropertyJsonParser;
import redis.clients.jedis.Jedis;
import redis.clients.jedis.params.ScanParams;
import redis.clients.jedis.resps.ScanResult;

/**
 * Implementation of ditributed cache to limit overhead, with REDIS (JEDIS).
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4jCacheManagerRedis implements FF4JCacheManager {

    /** Wrapping of redis connection (isolation). */
    private RedisConnection redisConnection;

    /** time to live for cache on top of store. */
    protected int timeToLive = 900000000;

    /** Default key builder. */
    private RedisKeysBuilder keyBuilder = new RedisKeysBuilder();
    
    /**
     * Constructors
     */
    public FF4jCacheManagerRedis() {
        this(new RedisConnection(), new RedisKeysBuilder());
    }
    public FF4jCacheManagerRedis(RedisKeysBuilder builder) {
        this(new RedisConnection(), builder);
    }
    public FF4jCacheManagerRedis(RedisConnection pRedisConnection) {
        this(pRedisConnection, new RedisKeysBuilder());
    }
    public FF4jCacheManagerRedis(RedisConnection pRedisConnection, RedisKeysBuilder builder) {
        this.redisConnection = pRedisConnection;
        this.keyBuilder      = builder;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> listCachedFeatureNames() {
        Jedis jedis = null;
        try {
            jedis = getJedis();
            return getKeys(jedis, keyBuilder.getKeyFeature("*"));
        } catch(RuntimeException re) {
            onException(re);
            return null;
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public String getCacheProviderName() {
        return "REDIS";
    }

    /** {@inheritDoc} */
    @Override
    public void clearFeatures() {
        Jedis jedis = null;
        try {
            jedis = getJedis();
            // --> This Pattern is not always supported
            // jedis.del(KEY_FEATURE + "*");
            // <--
            Set<String> matchingKeys = getKeys(jedis, keyBuilder.getKeyFeature("*"));

            if (!matchingKeys.isEmpty()) {
                jedis.del(matchingKeys.toArray(new String[matchingKeys.size()]));
            }
        } catch(RuntimeException re) {
            onException(re);
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
    }

    private Set<String> getKeys(Jedis jedis, String pattern) {
        Set<String> matchingKeys = new HashSet<>();
        try {
            ScanParams params = new ScanParams();
            params.match(pattern);
            String cursor = "0";
            do {
                ScanResult<String> scanResult = jedis.scan(cursor, params);
                List<String> keys = scanResult.getResult();
                cursor = scanResult.getCursor();
                matchingKeys.addAll(keys);
            } while (!cursor.equals("0"));
        } catch(RuntimeException re) {
            onException(re);
        } 
        return matchingKeys;
    }

    /** {@inheritDoc} */
    @Override
    public void clearProperties() {
        try ( Jedis jedis = getJedis() ) {
            // --> This Pattern is not always supported
            // jedis.del(KEY_PROPERTY + "*");
            // <--
            Set<String> matchingKeys = getKeys(jedis, keyBuilder.getKeyProperty("*"));

            if (!matchingKeys.isEmpty()) {
                jedis.del(matchingKeys.toArray(new String[matchingKeys.size()]));
            }
        } catch(RuntimeException re) {
            onException(re);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void evictFeature(String uid) {
        Util.assertParamHasLength(uid, " feature identifier");
        Jedis jedis = null;
        try {
            jedis = getJedis();
            jedis.del(keyBuilder.getKeyFeature(uid));
        } catch(RuntimeException re) {
            onException(re);
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public void evictProperty(String propertyName) {
        Util.assertParamHasLength(propertyName, " property name");
        if (propertyName == null || propertyName.isEmpty()) {
            throw new IllegalArgumentException("PropertyName cannot be null nor empty");
        }
        Jedis jedis = null;
        try {
            jedis = getJedis();
            jedis.del(keyBuilder.getKeyProperty(propertyName));
        } catch(RuntimeException re) {
            onException(re);
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    @SuppressWarnings("deprecation")
    public void putFeature(Feature fp) {
        Util.assertNotNull(fp);
        Jedis jedis = null;
        try {
            jedis = getJedis();
            jedis.set(keyBuilder.getKeyFeature(fp.getUid()), fp.toJson());
            jedis.expire(keyBuilder.getKeyFeature(fp.getUid()), getTimeToLive());
        } catch(RuntimeException re) {
            onException(re);
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    @SuppressWarnings("deprecation")
    public void putProperty(Property<?> property) {
        Util.assertNotNull(property);
        Jedis jedis = null;
        try {
            jedis = getJedis();
            jedis.set(keyBuilder.getKeyProperty(property.getName()), property.toJson());
            jedis.expire(keyBuilder.getKeyProperty(property.getName()), getTimeToLive());
        } catch(RuntimeException re) {
            onException(re);
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    public Feature getFeature(String uid) {
        Util.assertParamHasLength(uid, "feature uid");
        Jedis jedis = null;
        try {
            jedis = getJedis();
            String value = jedis.get(keyBuilder.getKeyFeature(uid));
            if (value != null) {
                return FeatureJsonParser.parseFeature(value);
            }
        } catch(RuntimeException re) {
            onException(re);
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> getProperty(String propertyName) {
        Util.assertParamHasLength(propertyName, "property name");
        Jedis jedis = null;
        try {
            jedis = getJedis();
            String value = jedis.get(keyBuilder.getKeyProperty(propertyName));
            if (value != null) {
                return PropertyJsonParser.parseProperty(value);
            }
        } catch(RuntimeException re) {
            onException(re);
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
        return null;
    }

    /** {@inheritDoc} */
    public Set<String> listCachedPropertyNames() {
        Jedis jedis = null;
        try {
            jedis = getJedis();
            return jedis.keys(keyBuilder.getKeyProperty("*"));
        } catch(RuntimeException re) {
            onException(re);
            return null;
        } finally {
            if (jedis != null) {
                jedis.close();
            }
        }
    }

    /** {@inheritDoc} */
    public Object getFeatureNativeCache() {
        return getJedis();
    }

    /** {@inheritDoc} */
    public Object getPropertyNativeCache() {
        return getJedis();
    }

    /**
     * Safe acces to Jedis, avoid JNPE.
     *
     * @return
     */
    public Jedis getJedis() {
        Util.assertNotNull(redisConnection);
        Jedis jedis = redisConnection.getJedis();
        if (jedis == null) {
            throw new IllegalArgumentException("Cannot found any jedis connection, please build connection");
        }
        return jedis;
    }

    /**
     * Getter accessor for attribute 'redisConnection'.
     *
     * @return
     *       current value of 'redisConnection'
     */
    public RedisConnection getRedisConnection() {
        return redisConnection;
    }

    /**
     * Setter accessor for attribute 'redisConnection'.
     * @param redisConnection
     * 		new value for 'redisConnection '
     */
    public void setRedisConnection(RedisConnection redisConnection) {
        this.redisConnection = redisConnection;
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
