package org.ff4j.redis.clientsidecache;

/*-
 * #%L
 * ff4j-store-redis
 * %%
 * Copyright (C) 2013 - 2023 FF4J
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

import io.lettuce.core.*;
import io.lettuce.core.api.sync.RedisKeyCommands;
import io.lettuce.core.api.sync.RedisStringCommands;
import io.lettuce.core.output.KeyValueStreamingChannel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.invoke.MethodHandles;
import java.util.List;
import java.util.Map;

public class ClientSideCacheRedisStringCommands<K, V> implements RedisStringCommands<K, V> {

    private static final Logger LOGGER = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

    private final RedisStringCommands<K, V> delegate;
    private final RedisKeyCommands<K, V> redisKeyCommands;
    private final RedisClientSideCache<K, V> clientSideCache;

    public ClientSideCacheRedisStringCommands(RedisStringCommands<K, V> delegate, RedisKeyCommands<K, V> redisKeyCommands, RedisClientSideCache<K, V> clientSideCache) {
        this.delegate = delegate;
        this.redisKeyCommands = redisKeyCommands;
        this.clientSideCache = clientSideCache;
    }

    @Override
    public Long append(K key, V value) {
        LOGGER.warn("append is not currently supported by client side caching");
        return delegate.append(key, value);
    }

    @Override
    public Long bitcount(K key) {
        LOGGER.warn("bitcount is not currently supported by client side caching");
        return delegate.bitcount(key);
    }

    @Override
    public Long bitcount(K key, long start, long end) {
        LOGGER.warn("bitcount is not currently supported by client side caching");
        return delegate.bitcount(key, start, end);
    }

    @Override
    public List<Long> bitfield(K key, BitFieldArgs bitFieldArgs) {
        LOGGER.warn("bitfield is not currently supported by client side caching");
        return delegate.bitfield(key, bitFieldArgs);
    }

    @Override
    public Long bitpos(K key, boolean state) {
        LOGGER.warn("bitpos is not currently supported by client side caching");
        return delegate.bitpos(key, state);
    }

    @Override
    public Long bitpos(K key, boolean state, long start) {
        LOGGER.warn("bitpos is not currently supported by client side caching");
        return delegate.bitpos(key, state, start);
    }

    @Override
    public Long bitpos(K key, boolean state, long start, long end) {
        LOGGER.warn("bitpos is not currently supported by client side caching");
        return delegate.bitpos(key, state, start, end);
    }

    @Override
    public Long bitopAnd(K destination, K... keys) {
        LOGGER.warn("bitopAnd is not currently supported by client side caching");
        return delegate.bitopAnd(destination, keys);
    }

    @Override
    public Long bitopNot(K destination, K source) {
        LOGGER.warn("bitopNot is not currently supported by client side caching");
        return delegate.bitopNot(destination, source);
    }

    @Override
    public Long bitopOr(K destination, K... keys) {
        LOGGER.warn("bitopOr is not currently supported by client side caching");
        return delegate.bitopOr(destination, keys);
    }

    @Override
    public Long bitopXor(K destination, K... keys) {
        LOGGER.warn("bitopXor is not currently supported by client side caching");
        return delegate.bitopXor(destination, keys);
    }

    @Override
    public Long decr(K key) {
        LOGGER.warn("decr is not currently supported by client side caching");
        return delegate.decr(key);
    }

    @Override
    public Long decrby(K key, long amount) {
        LOGGER.warn("decrby is not currently supported by client side caching");
        return delegate.decrby(key, amount);
    }

    @Override
    public V get(K key) {
        V value = clientSideCache.get(key);
        if (value == null) {
            value = delegate.get(key);
            if (value != null) {
                long ttl = redisKeyCommands.ttl(key);
                // Don't cache a value with no expiry time.
                if (ttl >= 0) {
                    clientSideCache.put(key, value, ttl);
                }
            }
        }
        return value;
    }

    @Override
    public Long getbit(K key, long offset) {
        LOGGER.warn("getbit is not currently supported by client side caching");
        return delegate.getbit(key, offset);
    }

    @Override
    public V getdel(K key) {
        LOGGER.warn("getdel is not currently supported by client side caching");
        return delegate.getdel(key);
    }

    @Override
    public V getex(K key, GetExArgs args) {
        LOGGER.warn("getex is not currently supported by client side caching");
        return delegate.getex(key, args);
    }

    @Override
    public V getrange(K key, long start, long end) {
        LOGGER.warn("getrange is not currently supported by client side caching");
        return delegate.getrange(key, start, end);
    }

    @Override
    public V getset(K key, V value) {
        LOGGER.warn("getset is not currently supported by client side caching");
        return delegate.getset(key, value);
    }

    @Override
    public Long incr(K key) {
        LOGGER.warn("incr is not currently supported by client side caching");
        return delegate.incr(key);
    }

    @Override
    public Long incrby(K key, long amount) {
        LOGGER.warn("incrby is not currently supported by client side caching");
        return delegate.incrby(key, amount);
    }

    @Override
    public Double incrbyfloat(K key, double amount) {
        LOGGER.warn("incrbyfloat is not currently supported by client side caching");
        return delegate.incrbyfloat(key, amount);
    }

    @Override
    public List<KeyValue<K, V>> mget(K... keys) {
        LOGGER.warn("mget is not currently supported by client side caching");
        return delegate.mget(keys);
    }

    @Override
    public Long mget(KeyValueStreamingChannel<K, V> channel, K... keys) {
        LOGGER.warn("mget is not currently supported by client side caching");
        return delegate.mget(channel, keys);
    }

    @Override
    public String mset(Map<K, V> map) {
        LOGGER.warn("mset is not currently supported by client side caching");
        return delegate.mset(map);
    }

    @Override
    public Boolean msetnx(Map<K, V> map) {
        LOGGER.warn("msetnx is not currently supported by client side caching");
        return delegate.msetnx(map);
    }

    @Override
    public String set(K key, V value) {
        // We could store the value in the clientSideCache immediately, but we wouldn't have the TTL information.
        // So we just set the value in Redis server, and the expiry time will be retrieved during the next "get".
        return delegate.set(key, value);
    }

    @Override
    public String set(K key, V value, SetArgs setArgs) {
        LOGGER.warn("set is not currently supported by client side caching");
        return delegate.set(key, value, setArgs);
    }

    @Override
    public V setGet(K key, V value) {
        LOGGER.warn("setGet is not currently supported by client side caching");
        return delegate.setGet(key, value);
    }

    @Override
    public V setGet(K key, V value, SetArgs setArgs) {
        LOGGER.warn("setGet is not currently supported by client side caching");
        return delegate.setGet(key, value, setArgs);
    }

    @Override
    public Long setbit(K key, long offset, int value) {
        LOGGER.warn("setbit is not currently supported by client side caching");
        return delegate.setbit(key, offset, value);
    }

    @Override
    public String setex(K key, long seconds, V value) {
        LOGGER.warn("setex is not currently supported by client side caching");
        return delegate.setex(key, seconds, value);
    }

    @Override
    public String psetex(K key, long milliseconds, V value) {
        LOGGER.warn("psetex is not currently supported by client side caching");
        return delegate.psetex(key, milliseconds, value);
    }

    @Override
    public Boolean setnx(K key, V value) {
        LOGGER.warn("setnx is not currently supported by client side caching");
        return delegate.setnx(key, value);
    }

    @Override
    public Long setrange(K key, long offset, V value) {
        LOGGER.warn("setrange is not currently supported by client side caching");
        return delegate.setrange(key, offset, value);
    }

    @Override
    public StringMatchResult stralgoLcs(StrAlgoArgs strAlgoArgs) {
        LOGGER.warn("stralgoLcs is not currently supported by client side caching");
        return delegate.stralgoLcs(strAlgoArgs);
    }

    @Override
    public Long strlen(K key) {
        LOGGER.warn("strlen is not currently supported by client side caching");
        return delegate.strlen(key);
    }
}
