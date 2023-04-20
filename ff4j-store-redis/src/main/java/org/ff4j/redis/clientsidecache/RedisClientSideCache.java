package org.ff4j.redis.clientsidecache;

import io.lettuce.core.RedisChannelHandler;
import io.lettuce.core.RedisConnectionStateListener;
import io.lettuce.core.StatefulRedisConnectionImpl;
import io.lettuce.core.TrackingArgs;
import io.lettuce.core.api.StatefulRedisConnection;
import io.lettuce.core.codec.RedisCodec;
import org.ff4j.cache.InMemoryCacheEntry;

import java.net.SocketAddress;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * A local cache implementation which listens to Redis Server key invalidation messages.
 */
public class RedisClientSideCache<K, V> {

    private final ConcurrentMap<K, InMemoryCacheEntry<V>> localCache = new ConcurrentHashMap<>();

    public RedisClientSideCache(StatefulRedisConnection<K, V> redisConnection) {
        setupRedisClientTracking(redisConnection);
    }

    public V get(K key) {
        InMemoryCacheEntry<V> localCacheEntry = localCache.get(key);
        if (localCacheEntry != null && !localCacheEntry.hasReachTimeToLive()) {
            return localCacheEntry.getEntry();
        }
        return null;
    }

    public void put(K key, V value) {
        localCache.put(key, new InMemoryCacheEntry<>(value));
    }

    public void expire(K key, long ttlInSec) {
        InMemoryCacheEntry<V> v = localCache.get(key);
        if (v != null) {
            localCache.put(key, new InMemoryCacheEntry<>(v.getEntry(), ttlInSec));
        }
    }

    public void remove(K k) {
        localCache.remove(k);
    }

    private void setupRedisClientTracking(StatefulRedisConnection<K, V> redisConnection) {
        StatefulRedisConnectionImpl<K, V> connectionImpl = (StatefulRedisConnectionImpl<K, V>) redisConnection;
        RedisCodec<K, V> codec = connectionImpl.getCodec();

        // Enable Redis client tracking
        redisConnection.sync().clientTracking(TrackingArgs.Builder.enabled());

        // Listen for invalidation message to keep local cache in "near-realtime" sync with server
        redisConnection.addListener(message -> {
            if (message.getType().equals("invalidate")) {

                List<Object> content = message.getContent(codec::decodeKey);
                List<K> keys = (List<K>) content.get(1);
                keys.forEach(localCache::remove);
            }
        });

        // Handle reconnection event : re-enable client tracking and flush local cache
        redisConnection.addListener(new RedisConnectionStateListener() {
            @Override
            public void onRedisConnected(RedisChannelHandler<?, ?> channelHandler, SocketAddress socketAddress) {
                // Setup tracking on freshly created connection
                if (channelHandler instanceof StatefulRedisConnection<?, ?> connection) {
                    connection.async().clientTracking(TrackingArgs.Builder.enabled());
                }
                // Remove previously stored entries that are not tracked anymore by Redis server since we're on a new connection.
                localCache.clear();
            }
        });
    }
}
