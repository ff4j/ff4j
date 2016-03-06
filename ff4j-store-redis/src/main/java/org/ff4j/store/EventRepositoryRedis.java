package org.ff4j.store;

import java.util.Set;

import org.ff4j.audit.Event;
import org.ff4j.audit.graph.BarChart;
import org.ff4j.audit.graph.PieChart;
import org.ff4j.audit.repository.AbstractEventRepository;
import org.ff4j.redis.RedisConnection;

/**
 * Implementation of the repository for redis.
 *
 * @author clunven
 */
public class EventRepositoryRedis extends AbstractEventRepository {

	/** prefix of keys. */
    public static String KEY_PROPERTY = "FF4J_EVENT_";
    
    /** default ttl. */
    private static int DEFAULT_TTL = 900000000;
    
    /** time to live. */
    protected int timeToLive = DEFAULT_TTL;
    
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
	public boolean saveEvent(Event e) {
		return false;
	}

	/** {@inheritDoc} */
	public Set<String> getFeatureNames() {
		return null;
	}

	/** {@inheritDoc} */
	public PieChart getHitsPieChart(long startTime, long endTime) {
		return null;
	}

	/** {@inheritDoc} */
	public BarChart getHitsBarChart(Set<String> featNameSet, long startTime, long endTime, int nbslot) {
		return null;
	}

	/** {@inheritDoc} */
	public PieChart getFeatureHitsPie(String featureId, long startTime, long endTime) {
		return null;
	}

	/** {@inheritDoc} */
	public int getTotalEventCount() {
		return 0;
	}

}
