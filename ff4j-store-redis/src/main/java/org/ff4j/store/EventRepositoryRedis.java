package org.ff4j.store;

import static org.ff4j.redis.RedisContants.KEY_EVENT;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.EventSeries;
import org.ff4j.audit.MutableHitCount;
import org.ff4j.audit.chart.TimeSeriesChart;
import org.ff4j.audit.repository.AbstractEventRepository;
import org.ff4j.redis.RedisConnection;

import redis.clients.jedis.Jedis;

/**
 * Persist audit events into REDIS storage technology.
 *
 * @author clunven
 */
public class EventRepositoryRedis extends AbstractEventRepository {
	
    /** Wrapping of redis connection (isolation). */
    private RedisConnection redisConnection;
    
	/**
     * Default Constructor.
     */
    public EventRepositoryRedis() {
        this(new RedisConnection());
    }
    
    /**
     * Contact remote redis server.
     * 
     * @param host
     *            target redis host
     * @param port
     *            target redis port
     */
    public EventRepositoryRedis(RedisConnection pRedisConnection) {
        redisConnection = pRedisConnection;
    }

    /**
     * Contact remote redis server.
     * 
     * @param host
     *            target redis host
     * @param port
     *            target redis port
     */
    public EventRepositoryRedis(String host, int port) {
        this(new RedisConnection(host, port));
    }
    
    /**
     * Contact remote redis server.
     * 
     * @param host
     *            target redis host
     * @param port
     *            target redis port
     */
    public EventRepositoryRedis(String host, int port, String password) {
        this(new RedisConnection(host, port, password));
    }
    
	/** {@inheritDoc} */
	public boolean saveEvent(Event evt) {
		 if (evt == null) {
			 throw new IllegalArgumentException("Event cannot be null nor empty");
	     }
		 Jedis jedis = null;
	     try {
	         jedis = getJedis();
    		 String uid = KEY_EVENT + "-" + evt.getTimestamp() + "-" + evt.getUuid();
    	     jedis.set(uid, evt.toJson());
    	     jedis.persist(uid);
    		 return true;
	        } finally {
	            if (jedis != null) {
	                jedis.close();
	            }
	        }
	}
    
    /**
     * Safe acces to Jedis, avoid JNPE.
     *
     * @return
     *      access jedis
     */
    public Jedis getJedis() {
        if (redisConnection == null) {
            throw new IllegalArgumentException("Cannot found any redisConnection");
        }
        Jedis jedis = redisConnection.getJedis();
        if (jedis == null) {
            throw new IllegalArgumentException("Cannot found any jedis connection, please build connection");
        }
        return jedis;
    }

    @Override
    public Map<String, MutableHitCount> getFeatureUsageHitCount(long startTime, long endTime) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public TimeSeriesChart getFeatureUsageHistory(long startTime, long endTime, TimeUnit tu) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public TimeSeriesChart getFeatureUsageHistory(long startTime, long endTime, TimeUnit tu, Set<String> filteredFeatures) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public EventSeries searchFeatureUsageEvents(EventQueryDefinition query) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void purgeFeatureUsage(long starTime, long endTime) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public Map<String, MutableHitCount> getHostHitCount(long startTime, long endTime) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Map<String, MutableHitCount> getUserHitCount(long startTime, long endTime) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Map<String, MutableHitCount> getSourceHitCount(long startTime, long endTime) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public TimeSeriesChart getAverageResponseTime(long startTime, long endTime, TimeUnit tu) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public TimeSeriesChart getAverageResponseTime(long startTime, long endTime, TimeUnit tu, Set<String> filteredFeatures) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public EventSeries getAuditTrail(long startTime, long endTime) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void purgeAuditTrail(long starTime, long endTime) {
        // TODO Auto-generated method stub
        
    }
}