package org.ff4j.cache.it;

/*
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
import io.lettuce.core.api.StatefulRedisConnection;
import io.lettuce.core.api.sync.RedisCommands;
import io.lettuce.core.api.sync.RedisKeyCommands;
import io.lettuce.core.api.sync.RedisStringCommands;
import io.lettuce.core.codec.RedisCodec;
import io.lettuce.core.output.ArrayOutput;
import io.lettuce.core.output.KeyStreamingChannel;
import io.lettuce.core.output.KeyValueStreamingChannel;
import io.lettuce.core.output.ValueStreamingChannel;
import io.lettuce.core.protocol.CommandArgs;
import org.ff4j.redis.clientsidecache.ClientSideCacheRedisKeyCommands;
import org.ff4j.redis.clientsidecache.ClientSideCacheRedisStringCommands;
import org.ff4j.redis.clientsidecache.RedisClientSideCache;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import java.time.Duration;
import java.time.Instant;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.TimeUnit;

import static io.lettuce.core.protocol.CommandType.CLIENT;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

@Ignore
public class RedisClientSideCacheIT {

    private static RedisClient redisClient;
    private static StatefulRedisConnection<String, String> redisConnection;

    private static RedisKeyCommands<String, String> clientSideCacheRedisKeyCommands;
    private static RedisStringCommands<String, String> clientSideCacheRedisStringCommands;

    private static CountingCallsRedisKeyCommands<String, String> countingCallsRedisKeyCommands;
    private static CountingCallsRedisStringCommands<String, String> countingCallsRedisStringCommands;

    @BeforeClass
    public static void setupRedisClient() {
        // Create Redis client
        redisClient = RedisClient.create("redis://localhost");
        redisConnection = redisClient.connect();

        // Add counting calls wrappers
        countingCallsRedisKeyCommands = new CountingCallsRedisKeyCommands<>(redisConnection.sync());
        countingCallsRedisStringCommands = new CountingCallsRedisStringCommands<>(redisConnection.sync());

        // Setup client side caching
        RedisClientSideCache<String, String> clientSideCache = new RedisClientSideCache<>(redisConnection);
        clientSideCacheRedisStringCommands = new ClientSideCacheRedisStringCommands<>(countingCallsRedisStringCommands, countingCallsRedisKeyCommands, clientSideCache);
        clientSideCacheRedisKeyCommands = new ClientSideCacheRedisKeyCommands<>(countingCallsRedisKeyCommands, clientSideCache);
    }

    @Before
    public void setupRedisContent() {
        // Empty redis in a separate connection
        RedisCommands<String, String> redisCommands = redisClient.connect().sync();
        redisCommands.scan().getKeys().forEach(clientSideCacheRedisKeyCommands::del);

        // Add some keys
        redisCommands.set("k1", "v1");
        redisCommands.expire("k1", 10);
        redisCommands.set("k2", "v2");
        redisCommands.expire("k2", 10);

        countingCallsRedisKeyCommands.resetCallsCount();
        countingCallsRedisStringCommands.resetCallsCount();
    }

    @Test
    public void ensureConnectionIsInTrackingMode() {
        // Crafting a custom REDIS command as CLIENT TRACKINGINFO is not yet supported by Lettuce
        // Given
        StatefulRedisConnectionImpl<String, String> connectionImpl = (StatefulRedisConnectionImpl<String, String>) redisConnection;
        RedisCodec<String, String> codec = connectionImpl.getCodec();
        CommandArgs<String, String> args = new CommandArgs<>(codec).add("TRACKINGINFO");

        // When
        List<Object> result = redisConnection.sync().dispatch(CLIENT, new ArrayOutput<>(codec), args);

        // Then
        List<String> flagsList = (List<String>) result.get(1);
        assertEquals("Client tracking should be ON", "on", flagsList.get(0));
    }


    @Test
    public void ensureOnlyOneCallIsDoneForGet() {
        // Given
        int getCount = countingCallsRedisStringCommands.getCallsCount("get");
        assertEquals("No external get count", 0, getCount);
        int setCount = countingCallsRedisStringCommands.getCallsCount("set");
        assertEquals("No external set count", 0, setCount);

        // When
        String value = clientSideCacheRedisStringCommands.get("k1");
        // Then
        assertEquals("v1", value);
        getCount = countingCallsRedisStringCommands.getCallsCount("get");
        assertEquals("First get count", 1, getCount);

        // When
        value = clientSideCacheRedisStringCommands.get("k1");
        // Then
        assertEquals("v1", value);
        getCount = countingCallsRedisStringCommands.getCallsCount("get");
        assertEquals("Still only one get count", 1, getCount);
    }


    @Test
    public void ensureLocalSetImpliesOnlyOneRemoteGet() {
        // Given
        int getCount = countingCallsRedisStringCommands.getCallsCount("get");
        assertEquals("No external get count", 0, getCount);
        int setCount = countingCallsRedisStringCommands.getCallsCount("set");
        assertEquals("No external set count", 0, setCount);

        // When
        clientSideCacheRedisStringCommands.set("newkey", "newvalue");
        clientSideCacheRedisKeyCommands.expire("newkey", 10);

        // Then
        getCount = countingCallsRedisStringCommands.getCallsCount("set");
        assertEquals("First set count", 1, getCount);

        // When
        String value = clientSideCacheRedisStringCommands.get("newkey");

        // Then
        assertEquals("newvalue", value);
        getCount = countingCallsRedisStringCommands.getCallsCount("get");
        assertEquals("Only one external get count", 1, getCount);

        // When
        value = clientSideCacheRedisStringCommands.get("newkey");

        // Then
        assertEquals("newvalue", value);
        getCount = countingCallsRedisStringCommands.getCallsCount("get");
        assertEquals("Still only one get count", 1, getCount);
    }


    @Test
    public void ensureClientSideCacheIsInvalidatedWhenValueIsChanged() throws InterruptedException {
        // Given
        int getCount = countingCallsRedisStringCommands.getCallsCount("get");
        assertEquals("No external get count", 0, getCount);

        // When
        String value = clientSideCacheRedisStringCommands.get("k1");
        // Then
        assertEquals("v1", value);
        getCount = countingCallsRedisStringCommands.getCallsCount("get");
        assertEquals("First get count", 1, getCount);

        // Triggering a value change using another connection
        RedisCommands<String, String> redisCommands = redisClient.connect().sync();
        redisCommands.set("k1", "v1.1");
        redisCommands.expire("k1", 10);

        // Sleeping to ensure propagation
        TimeUnit.MILLISECONDS.sleep(250);

        // When
        value = clientSideCacheRedisStringCommands.get("k1");
        // Then
        assertEquals("v1.1", value);
        getCount = countingCallsRedisStringCommands.getCallsCount("get");
        assertEquals("Another get was issued after invalidation", 2, getCount);
    }

    @Test
    public void ensureClientSideCacheHonorsRemoteExpiration() throws InterruptedException {
        // Given
        // We act as another client setting an expiry time.
        RedisCommands<String, String> redisCommands = redisClient.connect().sync();
        redisCommands.set("k1", "v1");
        redisCommands.expire("k1", 1);

        // Given
        int getCount = countingCallsRedisStringCommands.getCallsCount("get");
        assertEquals("No external get count", 0, getCount);

        // When
        String value = clientSideCacheRedisStringCommands.get("k1");
        // Then
        assertEquals("v1", value);
        getCount = countingCallsRedisStringCommands.getCallsCount("get");
        assertEquals("First get count", 1, getCount);

        // When
        TimeUnit.MILLISECONDS.sleep(1200);

        // When
        value = clientSideCacheRedisStringCommands.get("k1");
        // Then
        assertNull(value);
        getCount = countingCallsRedisStringCommands.getCallsCount("get");
        assertEquals("Another get was issued after invalidation", 2, getCount);
    }

    @Test
    public void ensureClientSideCacheHonorsLocalExpiration() throws InterruptedException {
        // Given
        int expireCount = countingCallsRedisKeyCommands.getCallsCount("expire");
        assertEquals("No external expire count", 0, expireCount);
        String value = clientSideCacheRedisStringCommands.get("k1");
        assertEquals("v1", value);
        int getCount = countingCallsRedisStringCommands.getCallsCount("get");
        assertEquals("First get count", 1, getCount);
        countingCallsRedisStringCommands.resetCallsCount();

        // When
        clientSideCacheRedisKeyCommands.expire("k1", 1);
        TimeUnit.MILLISECONDS.sleep(1200);

        // Then : we should have an expire call to Redis Server
        expireCount = countingCallsRedisKeyCommands.getCallsCount("expire");
        assertEquals("One expire call", 1, expireCount);

        // When
        value = clientSideCacheRedisStringCommands.get("k1");

        // Then
        assertNull(value);
        getCount = countingCallsRedisStringCommands.getCallsCount("get");
        assertEquals("No local entry, one get to try to fetch value", 1, getCount);
    }


    public static class CountingCallsRedisKeyCommands<K, V> implements RedisKeyCommands<K, V> {
        private final RedisKeyCommands<K, V> delegate;

        private final ConcurrentMap<String, Integer> methodCallsMap = new ConcurrentHashMap<>();

        public CountingCallsRedisKeyCommands(RedisKeyCommands<K, V> delegate) {
            this.delegate = delegate;
        }

        public int getCallsCount(String methodName) {
            Integer integer = methodCallsMap.get(methodName);
            if (integer == null) {
                return 0;
            }
            return integer;
        }

        public void resetCallsCount() {
            methodCallsMap.clear();
        }

        @Override
        public Boolean copy(K source, K destination) {
            return delegate.copy(source, destination);
        }

        @Override
        public Boolean copy(K source, K destination, CopyArgs copyArgs) {
            return delegate.copy(source, destination, copyArgs);
        }

        @Override
        public Long del(K... keys) {
            methodCallsMap.compute("del", (s, integer) -> integer == null ? 1 : ++integer);
            return delegate.del(keys);
        }

        @Override
        public Long unlink(K... keys) {
            return delegate.unlink(keys);
        }

        @Override
        public byte[] dump(K key) {
            return delegate.dump(key);
        }

        @Override
        public Long exists(K... keys) {
            return delegate.exists(keys);
        }

        @Override
        public Boolean expire(K key, long seconds) {
            methodCallsMap.compute("expire", (s, integer) -> integer == null ? 1 : ++integer);
            return delegate.expire(key, seconds);
        }

        @Override
        public Boolean expire(K key, long seconds, ExpireArgs expireArgs) {
            return delegate.expire(key, seconds, expireArgs);
        }

        @Override
        public Boolean expire(K key, Duration seconds) {
            return delegate.expire(key, seconds);
        }

        @Override
        public Boolean expire(K key, Duration seconds, ExpireArgs expireArgs) {
            return delegate.expire(key, seconds, expireArgs);
        }

        @Override
        public Boolean expireat(K key, long timestamp) {
            return delegate.expireat(key, timestamp);
        }

        @Override
        public Boolean expireat(K key, long timestamp, ExpireArgs expireArgs) {
            return delegate.expireat(key, timestamp, expireArgs);
        }

        @Override
        public Boolean expireat(K key, Date timestamp) {
            return delegate.expireat(key, timestamp);
        }

        @Override
        public Boolean expireat(K key, Date timestamp, ExpireArgs expireArgs) {
            return delegate.expireat(key, timestamp, expireArgs);
        }

        @Override
        public Boolean expireat(K key, Instant timestamp) {
            return delegate.expireat(key, timestamp);
        }

        @Override
        public Boolean expireat(K key, Instant timestamp, ExpireArgs expireArgs) {
            return delegate.expireat(key, timestamp, expireArgs);
        }

        @Override
        public Long expiretime(K key) {
            return delegate.expiretime(key);
        }

        @Override
        public List<K> keys(K pattern) {
            return delegate.keys(pattern);
        }

        @Override
        public Long keys(KeyStreamingChannel<K> channel, K pattern) {
            return delegate.keys(channel, pattern);
        }

        @Override
        public String migrate(String host, int port, K key, int db, long timeout) {
            return delegate.migrate(host, port, key, db, timeout);
        }

        @Override
        public String migrate(String host, int port, int db, long timeout, MigrateArgs<K> migrateArgs) {
            return delegate.migrate(host, port, db, timeout, migrateArgs);
        }

        @Override
        public Boolean move(K key, int db) {
            return delegate.move(key, db);
        }

        @Override
        public String objectEncoding(K key) {
            return delegate.objectEncoding(key);
        }

        @Override
        public Long objectFreq(K key) {
            return delegate.objectFreq(key);
        }

        @Override
        public Long objectIdletime(K key) {
            return delegate.objectIdletime(key);
        }

        @Override
        public Long objectRefcount(K key) {
            return delegate.objectRefcount(key);
        }

        @Override
        public Boolean persist(K key) {
            return delegate.persist(key);
        }

        @Override
        public Boolean pexpire(K key, long milliseconds) {
            return delegate.pexpire(key, milliseconds);
        }

        @Override
        public Boolean pexpire(K key, long milliseconds, ExpireArgs expireArgs) {
            return delegate.pexpire(key, milliseconds, expireArgs);
        }

        @Override
        public Boolean pexpire(K key, Duration milliseconds) {
            return delegate.pexpire(key, milliseconds);
        }

        @Override
        public Boolean pexpire(K key, Duration milliseconds, ExpireArgs expireArgs) {
            return delegate.pexpire(key, milliseconds, expireArgs);
        }

        @Override
        public Boolean pexpireat(K key, long timestamp) {
            return delegate.pexpireat(key, timestamp);
        }

        @Override
        public Boolean pexpireat(K key, long timestamp, ExpireArgs expireArgs) {
            return delegate.pexpireat(key, timestamp, expireArgs);
        }

        @Override
        public Boolean pexpireat(K key, Date timestamp) {
            return delegate.pexpireat(key, timestamp);
        }

        @Override
        public Boolean pexpireat(K key, Date timestamp, ExpireArgs expireArgs) {
            return delegate.pexpireat(key, timestamp, expireArgs);
        }

        @Override
        public Boolean pexpireat(K key, Instant timestamp) {
            return delegate.pexpireat(key, timestamp);
        }

        @Override
        public Boolean pexpireat(K key, Instant timestamp, ExpireArgs expireArgs) {
            return delegate.pexpireat(key, timestamp, expireArgs);
        }

        @Override
        public Long pexpiretime(K key) {
            return delegate.pexpiretime(key);
        }

        @Override
        public Long pttl(K key) {
            return delegate.pttl(key);
        }

        @Override
        public K randomkey() {
            return delegate.randomkey();
        }

        @Override
        public String rename(K key, K newKey) {
            return delegate.rename(key, newKey);
        }

        @Override
        public Boolean renamenx(K key, K newKey) {
            return delegate.renamenx(key, newKey);
        }

        @Override
        public String restore(K key, long ttl, byte[] value) {
            return delegate.restore(key, ttl, value);
        }

        @Override
        public String restore(K key, byte[] value, RestoreArgs args) {
            return delegate.restore(key, value, args);
        }

        @Override
        public List<V> sort(K key) {
            return delegate.sort(key);
        }

        @Override
        public Long sort(ValueStreamingChannel<V> channel, K key) {
            return delegate.sort(channel, key);
        }

        @Override
        public List<V> sort(K key, SortArgs sortArgs) {
            return delegate.sort(key, sortArgs);
        }

        @Override
        public Long sort(ValueStreamingChannel<V> channel, K key, SortArgs sortArgs) {
            return delegate.sort(channel, key, sortArgs);
        }

        @Override
        public List<V> sortReadOnly(K key) {
            return delegate.sortReadOnly(key);
        }

        @Override
        public Long sortReadOnly(ValueStreamingChannel<V> channel, K key) {
            return delegate.sortReadOnly(channel, key);
        }

        @Override
        public List<V> sortReadOnly(K key, SortArgs sortArgs) {
            return delegate.sortReadOnly(key, sortArgs);
        }

        @Override
        public Long sortReadOnly(ValueStreamingChannel<V> channel, K key, SortArgs sortArgs) {
            return delegate.sortReadOnly(channel, key, sortArgs);
        }

        @Override
        public Long sortStore(K key, SortArgs sortArgs, K destination) {
            return delegate.sortStore(key, sortArgs, destination);
        }

        @Override
        public Long touch(K... keys) {
            return delegate.touch(keys);
        }

        @Override
        public Long ttl(K key) {
            return delegate.ttl(key);
        }

        @Override
        public String type(K key) {
            return delegate.type(key);
        }

        @Override
        public KeyScanCursor<K> scan() {
            return delegate.scan();
        }

        @Override
        public KeyScanCursor<K> scan(ScanArgs scanArgs) {
            return delegate.scan(scanArgs);
        }

        @Override
        public KeyScanCursor<K> scan(ScanCursor scanCursor, ScanArgs scanArgs) {
            return delegate.scan(scanCursor, scanArgs);
        }

        @Override
        public KeyScanCursor<K> scan(ScanCursor scanCursor) {
            return delegate.scan(scanCursor);
        }

        @Override
        public StreamScanCursor scan(KeyStreamingChannel<K> channel) {
            return delegate.scan(channel);
        }

        @Override
        public StreamScanCursor scan(KeyStreamingChannel<K> channel, ScanArgs scanArgs) {
            return delegate.scan(channel, scanArgs);
        }

        @Override
        public StreamScanCursor scan(KeyStreamingChannel<K> channel, ScanCursor scanCursor, ScanArgs scanArgs) {
            return delegate.scan(channel, scanCursor, scanArgs);
        }

        @Override
        public StreamScanCursor scan(KeyStreamingChannel<K> channel, ScanCursor scanCursor) {
            return delegate.scan(channel, scanCursor);
        }
    }

    public static class CountingCallsRedisStringCommands<K, V> implements RedisStringCommands<K, V> {
        private final RedisStringCommands<K, V> delegate;

        private final ConcurrentMap<String, Integer> methodCallsMap = new ConcurrentHashMap<>();

        public CountingCallsRedisStringCommands(RedisStringCommands<K, V> delegate) {
            this.delegate = delegate;
        }

        public int getCallsCount(String methodName) {
            Integer integer = methodCallsMap.get(methodName);
            if (integer == null) {
                return 0;
            }
            return integer;
        }

        public void resetCallsCount() {
            methodCallsMap.clear();
        }

        @Override
        public Long append(K key, V value) {
            return delegate.append(key, value);
        }

        @Override
        public Long bitcount(K key) {
            return delegate.bitcount(key);
        }

        @Override
        public Long bitcount(K key, long start, long end) {
            return delegate.bitcount(key, start, end);
        }

        @Override
        public List<Long> bitfield(K key, BitFieldArgs bitFieldArgs) {
            return delegate.bitfield(key, bitFieldArgs);
        }

        @Override
        public Long bitpos(K key, boolean state) {
            return delegate.bitpos(key, state);
        }

        @Override
        public Long bitpos(K key, boolean state, long start) {
            return delegate.bitpos(key, state, start);
        }

        @Override
        public Long bitpos(K key, boolean state, long start, long end) {
            return delegate.bitpos(key, state, start, end);
        }

        @Override
        public Long bitopAnd(K destination, K... keys) {
            return delegate.bitopAnd(destination, keys);
        }

        @Override
        public Long bitopNot(K destination, K source) {
            return delegate.bitopNot(destination, source);
        }

        @Override
        public Long bitopOr(K destination, K... keys) {
            return delegate.bitopOr(destination, keys);
        }

        @Override
        public Long bitopXor(K destination, K... keys) {
            return delegate.bitopXor(destination, keys);
        }

        @Override
        public Long decr(K key) {
            return delegate.decr(key);
        }

        @Override
        public Long decrby(K key, long amount) {
            return delegate.decrby(key, amount);
        }

        @Override
        public V get(K key) {
            methodCallsMap.compute("get", (s, integer) -> integer == null ? 1 : ++integer);
            return delegate.get(key);
        }

        @Override
        public Long getbit(K key, long offset) {
            return delegate.getbit(key, offset);
        }

        @Override
        public V getdel(K key) {
            return delegate.getdel(key);
        }

        @Override
        public V getex(K key, GetExArgs args) {
            return delegate.getex(key, args);
        }

        @Override
        public V getrange(K key, long start, long end) {
            return delegate.getrange(key, start, end);
        }

        @Override
        public V getset(K key, V value) {
            return delegate.getset(key, value);
        }

        @Override
        public Long incr(K key) {
            return delegate.incr(key);
        }

        @Override
        public Long incrby(K key, long amount) {
            return delegate.incrby(key, amount);
        }

        @Override
        public Double incrbyfloat(K key, double amount) {
            return delegate.incrbyfloat(key, amount);
        }

        @Override
        public List<KeyValue<K, V>> mget(K... keys) {
            return delegate.mget(keys);
        }

        @Override
        public Long mget(KeyValueStreamingChannel<K, V> channel, K... keys) {
            return delegate.mget(channel, keys);
        }

        @Override
        public String mset(Map<K, V> map) {
            return delegate.mset(map);
        }

        @Override
        public Boolean msetnx(Map<K, V> map) {
            return delegate.msetnx(map);
        }

        @Override
        public String set(K key, V value) {
            methodCallsMap.compute("set", (s, integer) -> integer == null ? 1 : ++integer);
            return delegate.set(key, value);
        }

        @Override
        public String set(K key, V value, SetArgs setArgs) {
            return delegate.set(key, value, setArgs);
        }

        @Override
        public V setGet(K key, V value) {
            return delegate.setGet(key, value);
        }

        @Override
        public V setGet(K key, V value, SetArgs setArgs) {
            return delegate.setGet(key, value, setArgs);
        }

        @Override
        public Long setbit(K key, long offset, int value) {
            return delegate.setbit(key, offset, value);
        }

        @Override
        public String setex(K key, long seconds, V value) {
            return delegate.setex(key, seconds, value);
        }

        @Override
        public String psetex(K key, long milliseconds, V value) {
            return delegate.psetex(key, milliseconds, value);
        }

        @Override
        public Boolean setnx(K key, V value) {
            return delegate.setnx(key, value);
        }

        @Override
        public Long setrange(K key, long offset, V value) {
            return delegate.setrange(key, offset, value);
        }

        @Override
        public StringMatchResult stralgoLcs(StrAlgoArgs strAlgoArgs) {
            return delegate.stralgoLcs(strAlgoArgs);
        }

        @Override
        public Long strlen(K key) {
            return delegate.strlen(key);
        }
    }
}
