package org.ff4j.store.it;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.property.PropertyString;
import org.ff4j.redis.RedisConnection;
import org.ff4j.store.EventRepositoryRedis;
import org.ff4j.store.FeatureStoreRedis;
import org.ff4j.store.PropertyStoreRedis;
import org.junit.Test;

/**
 * 
 * 
 * 
 * Class to TODO
 *
 * @author Cedrick Lunven (@clunven)
 *
 */
public class FF4JRedisTestIt {

    @Test
    public void testInitialiationRedis() {
       
        
        // Default connection paramaters
        RedisConnection redisConnection = new RedisConnection();
        
        // Initialization of FF4J
        FF4j ff4j = new FF4j().audit(true);
        ff4j.setFeatureStore(new FeatureStoreRedis(redisConnection));
        ff4j.setPropertiesStore(new PropertyStoreRedis(redisConnection));
        ff4j.setEventRepository(new EventRepositoryRedis(redisConnection));
        
        // Empty Store
        ff4j.getFeatureStore().clear();
        ff4j.getPropertiesStore().clear();
        
        // Create a feature
        Feature f1 = new Feature("f1", true, "My firts feature", "Group1");
        ff4j.getFeatureStore().create(f1);
        
        // create a property
        PropertyString p1 = new PropertyString("p1", "v1");
        ff4j.getPropertiesStore().createProperty(p1);
        
        ff4j.check("f1");
        
        
        // Start with an empty store
        //Assert.assertTrue(redisStore.exist("f1"));
        
        // If you want to import feature (ONLY ONCE)
        //redisStore.importFeaturesFromXmlFile("ff4j.xml");
        
    }
}
