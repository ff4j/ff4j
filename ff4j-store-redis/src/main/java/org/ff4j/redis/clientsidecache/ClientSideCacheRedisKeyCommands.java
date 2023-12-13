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
import io.lettuce.core.output.KeyStreamingChannel;
import io.lettuce.core.output.ValueStreamingChannel;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.invoke.MethodHandles;
import java.time.Duration;
import java.time.Instant;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

public class ClientSideCacheRedisKeyCommands<K, V> implements RedisKeyCommands<K, V> {

    private static final Logger LOGGER = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

    private final RedisKeyCommands<K, V> delegate;
    private final RedisClientSideCache<K, V> clientSideCache;

    public ClientSideCacheRedisKeyCommands(RedisKeyCommands<K, V> delegate, RedisClientSideCache<K, V> clientSideCache) {
        this.delegate = delegate;
        this.clientSideCache = clientSideCache;
    }

    @Override
    public Boolean copy(K source, K destination) {
        LOGGER.warn("copy is not currently supported by client side caching");
        return delegate.copy(source, destination);
    }

    @Override
    public Boolean copy(K source, K destination, CopyArgs copyArgs) {
        LOGGER.warn("copy is not currently supported by client side caching");
        return delegate.copy(source, destination, copyArgs);
    }

    @Override
    public Long del(K... keys) {
        Arrays.stream(keys).forEach(clientSideCache::remove);
        return delegate.del(keys);
    }

    @Override
    public Long unlink(K... keys) {
        LOGGER.warn("unlink is not currently supported by client side caching");
        return delegate.unlink(keys);
    }

    @Override
    public byte[] dump(K key) {
        LOGGER.warn("dump is not currently supported by client side caching");
        return delegate.dump(key);
    }

    @Override
    public Long exists(K... keys) {
        LOGGER.warn("exists is not currently supported by client side caching");
        return delegate.exists(keys);
    }

    @Override
    public Boolean expire(K key, long seconds) {
        clientSideCache.expire(key, seconds);
        return delegate.expire(key, seconds);
    }

    @Override
    public Boolean expire(K key, long seconds, ExpireArgs expireArgs) {
        LOGGER.warn("expire is not currently supported by client side caching");
        return delegate.expire(key, seconds, expireArgs);
    }

    @Override
    public Boolean expire(K key, Duration seconds) {
        LOGGER.warn("expire is not currently supported by client side caching");
        return delegate.expire(key, seconds);
    }

    @Override
    public Boolean expire(K key, Duration seconds, ExpireArgs expireArgs) {
        LOGGER.warn("expire is not currently supported by client side caching");
        return delegate.expire(key, seconds, expireArgs);
    }

    @Override
    public Boolean expireat(K key, long timestamp) {
        LOGGER.warn("expireat is not currently supported by client side caching");
        return delegate.expireat(key, timestamp);
    }

    @Override
    public Boolean expireat(K key, long timestamp, ExpireArgs expireArgs) {
        LOGGER.warn("expireat is not currently supported by client side caching");
        return delegate.expireat(key, timestamp, expireArgs);
    }

    @Override
    public Boolean expireat(K key, Date timestamp) {
        LOGGER.warn("expireat is not currently supported by client side caching");
        return delegate.expireat(key, timestamp);
    }

    @Override
    public Boolean expireat(K key, Date timestamp, ExpireArgs expireArgs) {
        LOGGER.warn("expireat is not currently supported by client side caching");
        return delegate.expireat(key, timestamp, expireArgs);
    }

    @Override
    public Boolean expireat(K key, Instant timestamp) {
        LOGGER.warn("expireat is not currently supported by client side caching");
        return delegate.expireat(key, timestamp);
    }

    @Override
    public Boolean expireat(K key, Instant timestamp, ExpireArgs expireArgs) {
        LOGGER.warn("expireat is not currently supported by client side caching");
        return delegate.expireat(key, timestamp, expireArgs);
    }

    @Override
    public Long expiretime(K key) {
        LOGGER.warn("expiretime is not currently supported by client side caching");
        return delegate.expiretime(key);
    }

    @Override
    public List<K> keys(K pattern) {
        LOGGER.warn("keys is not currently supported by client side caching");
        return delegate.keys(pattern);
    }

    @Override
    public Long keys(KeyStreamingChannel<K> channel, K pattern) {
        LOGGER.warn("keys is not currently supported by client side caching");
        return delegate.keys(channel, pattern);
    }

    @Override
    public String migrate(String host, int port, K key, int db, long timeout) {
        LOGGER.warn("migrate is not currently supported by client side caching");
        return delegate.migrate(host, port, key, db, timeout);
    }

    @Override
    public String migrate(String host, int port, int db, long timeout, MigrateArgs<K> migrateArgs) {
        LOGGER.warn("migrate is not currently supported by client side caching");
        return delegate.migrate(host, port, db, timeout, migrateArgs);
    }

    @Override
    public Boolean move(K key, int db) {
        LOGGER.warn("move is not currently supported by client side caching");
        return delegate.move(key, db);
    }

    @Override
    public String objectEncoding(K key) {
        LOGGER.warn("objectEncoding is not currently supported by client side caching");
        return delegate.objectEncoding(key);
    }

    @Override
    public Long objectFreq(K key) {
        LOGGER.warn("objectFreq is not currently supported by client side caching");
        return delegate.objectFreq(key);
    }

    @Override
    public Long objectIdletime(K key) {
        LOGGER.warn("objectIdletime is not currently supported by client side caching");
        return delegate.objectIdletime(key);
    }

    @Override
    public Long objectRefcount(K key) {
        LOGGER.warn("objectRefcount is not currently supported by client side caching");
        return delegate.objectRefcount(key);
    }

    @Override
    public Boolean persist(K key) {
        LOGGER.warn("persist is not currently supported by client side caching");
        return delegate.persist(key);
    }

    @Override
    public Boolean pexpire(K key, long milliseconds) {
        LOGGER.warn("pexpire is not currently supported by client side caching");
        return delegate.pexpire(key, milliseconds);
    }

    @Override
    public Boolean pexpire(K key, long milliseconds, ExpireArgs expireArgs) {
        LOGGER.warn("pexpire is not currently supported by client side caching");
        return delegate.pexpire(key, milliseconds, expireArgs);
    }

    @Override
    public Boolean pexpire(K key, Duration milliseconds) {
        LOGGER.warn("pexpire is not currently supported by client side caching");
        return delegate.pexpire(key, milliseconds);
    }

    @Override
    public Boolean pexpire(K key, Duration milliseconds, ExpireArgs expireArgs) {
        LOGGER.warn("pexpire is not currently supported by client side caching");
        return delegate.pexpire(key, milliseconds, expireArgs);
    }

    @Override
    public Boolean pexpireat(K key, long timestamp) {
        LOGGER.warn("pexpireat is not currently supported by client side caching");
        return delegate.pexpireat(key, timestamp);
    }

    @Override
    public Boolean pexpireat(K key, long timestamp, ExpireArgs expireArgs) {
        LOGGER.warn("pexpireat is not currently supported by client side caching");
        return delegate.pexpireat(key, timestamp, expireArgs);
    }

    @Override
    public Boolean pexpireat(K key, Date timestamp) {
        LOGGER.warn("pexpireat is not currently supported by client side caching");
        return delegate.pexpireat(key, timestamp);
    }

    @Override
    public Boolean pexpireat(K key, Date timestamp, ExpireArgs expireArgs) {
        LOGGER.warn("pexpireat is not currently supported by client side caching");
        return delegate.pexpireat(key, timestamp, expireArgs);
    }

    @Override
    public Boolean pexpireat(K key, Instant timestamp) {
        LOGGER.warn("pexpireat is not currently supported by client side caching");
        return delegate.pexpireat(key, timestamp);
    }

    @Override
    public Boolean pexpireat(K key, Instant timestamp, ExpireArgs expireArgs) {
        LOGGER.warn("pexpireat is not currently supported by client side caching");
        return delegate.pexpireat(key, timestamp, expireArgs);
    }

    @Override
    public Long pexpiretime(K key) {
        LOGGER.warn("pexpiretime is not currently supported by client side caching");
        return delegate.pexpiretime(key);
    }

    @Override
    public Long pttl(K key) {
        LOGGER.warn("pttl is not currently supported by client side caching");
        return delegate.pttl(key);
    }

    @Override
    public K randomkey() {
        LOGGER.warn("randomkey is not currently supported by client side caching");
        return delegate.randomkey();
    }

    @Override
    public String rename(K key, K newKey) {
        LOGGER.warn("rename is not currently supported by client side caching");
        return delegate.rename(key, newKey);
    }

    @Override
    public Boolean renamenx(K key, K newKey) {
        LOGGER.warn("renamenx is not currently supported by client side caching");
        return delegate.renamenx(key, newKey);
    }

    @Override
    public String restore(K key, long ttl, byte[] value) {
        LOGGER.warn("restore is not currently supported by client side caching");
        return delegate.restore(key, ttl, value);
    }

    @Override
    public String restore(K key, byte[] value, RestoreArgs args) {
        LOGGER.warn("restore is not currently supported by client side caching");
        return delegate.restore(key, value, args);
    }

    @Override
    public List<V> sort(K key) {
        LOGGER.warn("sort is not currently supported by client side caching");
        return delegate.sort(key);
    }

    @Override
    public Long sort(ValueStreamingChannel<V> channel, K key) {
        LOGGER.warn("sort is not currently supported by client side caching");
        return delegate.sort(channel, key);
    }

    @Override
    public List<V> sort(K key, SortArgs sortArgs) {
        LOGGER.warn("sort is not currently supported by client side caching");
        return delegate.sort(key, sortArgs);
    }

    @Override
    public Long sort(ValueStreamingChannel<V> channel, K key, SortArgs sortArgs) {
        LOGGER.warn("sort is not currently supported by client side caching");
        return delegate.sort(channel, key, sortArgs);
    }

    @Override
    public List<V> sortReadOnly(K key) {
        LOGGER.warn("sortReadOnly is not currently supported by client side caching");
        return delegate.sortReadOnly(key);
    }

    @Override
    public Long sortReadOnly(ValueStreamingChannel<V> channel, K key) {
        LOGGER.warn("sortReadOnly is not currently supported by client side caching");
        return delegate.sortReadOnly(channel, key);
    }

    @Override
    public List<V> sortReadOnly(K key, SortArgs sortArgs) {
        LOGGER.warn("sortReadOnly is not currently supported by client side caching");
        return delegate.sortReadOnly(key, sortArgs);
    }

    @Override
    public Long sortReadOnly(ValueStreamingChannel<V> channel, K key, SortArgs sortArgs) {
        LOGGER.warn("sortReadOnly is not currently supported by client side caching");
        return delegate.sortReadOnly(channel, key, sortArgs);
    }

    @Override
    public Long sortStore(K key, SortArgs sortArgs, K destination) {
        LOGGER.warn("sortStore is not currently supported by client side caching");
        return delegate.sortStore(key, sortArgs, destination);
    }

    @Override
    public Long touch(K... keys) {
        LOGGER.warn("touch is not currently supported by client side caching");
        return delegate.touch(keys);
    }

    @Override
    public Long ttl(K key) {
        LOGGER.warn("ttl is not currently supported by client side caching");
        return delegate.ttl(key);
    }

    @Override
    public String type(K key) {
        LOGGER.warn("type is not currently supported by client side caching");
        return delegate.type(key);
    }

    @Override
    public KeyScanCursor<K> scan() {
        LOGGER.warn("scan is not currently supported by client side caching");
        return delegate.scan();
    }

    @Override
    public KeyScanCursor<K> scan(ScanArgs scanArgs) {
        LOGGER.warn("scan is not currently supported by client side caching");
        return delegate.scan(scanArgs);
    }

    @Override
    public KeyScanCursor<K> scan(ScanCursor scanCursor, ScanArgs scanArgs) {
        LOGGER.warn("scan is not currently supported by client side caching");
        return delegate.scan(scanCursor, scanArgs);
    }

    @Override
    public KeyScanCursor<K> scan(ScanCursor scanCursor) {
        LOGGER.warn("scan is not currently supported by client side caching");
        return delegate.scan(scanCursor);
    }

    @Override
    public StreamScanCursor scan(KeyStreamingChannel<K> channel) {
        LOGGER.warn("scan is not currently supported by client side caching");
        return delegate.scan(channel);
    }

    @Override
    public StreamScanCursor scan(KeyStreamingChannel<K> channel, ScanArgs scanArgs) {
        LOGGER.warn("scan is not currently supported by client side caching");
        return delegate.scan(channel, scanArgs);
    }

    @Override
    public StreamScanCursor scan(KeyStreamingChannel<K> channel, ScanCursor scanCursor, ScanArgs scanArgs) {
        LOGGER.warn("scan is not currently supported by client side caching");
        return delegate.scan(channel, scanCursor, scanArgs);
    }

    @Override
    public StreamScanCursor scan(KeyStreamingChannel<K> channel, ScanCursor scanCursor) {
        LOGGER.warn("scan is not currently supported by client side caching");
        return delegate.scan(channel, scanCursor);
    }
}
