package org.ff4j.cassandra;

/*
 * #%L
 * ff4j-store-cassandra
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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

import java.lang.reflect.Constructor;

import org.cassandraunit.utils.EmbeddedCassandraServerHelper;
import org.ff4j.audit.EventConstants;
import org.ff4j.audit.EventQueryDefinition;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import com.datastax.driver.core.Cluster;
import com.datastax.driver.core.exceptions.NoHostAvailableException;

/**
 * Dummy tests t work with cassandra.
 * @author Cedrick LUNVEN (@clunven)
 *
 */
//Cassandra embedded KO
@Ignore
public class CassandraTest {
    
    /** Reuse the embedded server. */
    protected static CassandraConnection conn;
    
    @BeforeClass
    public static void startEmbeddedCassandra() throws Exception {
        EmbeddedCassandraServerHelper.startEmbeddedCassandra(15000);
        conn = new CassandraConnection("127.0.0.1", 9142);
        conn.createKeySpace();
    }
    
    /** TDD. */
    @Test
    public void testCassandraConstant() throws Exception {
        Constructor<CassandraConstants> ce = CassandraConstants.class.getDeclaredConstructor();
        ce.setAccessible(true);
        ce.newInstance();
    }
    
    /** TDD. */
    @Test
    public void testCassandraMapper() throws Exception {
        Constructor<CassandraMapper> ce = CassandraMapper.class.getDeclaredConstructor();
        ce.setAccessible(true);
        ce.newInstance();
        
    }
    
    @Test
    public void testCassandraQueryBuilder() {
        CassandraConnection   cc  = new CassandraConnection();
        CassandraQueryBuilder cqb = new CassandraQueryBuilder(cc);

        // Mapping with a whole set of filters
        EventQueryDefinition eqd = new EventQueryDefinition();
        eqd.addFilterAction(EventConstants.ACTION_CLEAR);
        eqd.addFilterHost("localhost");
        eqd.addFilterName("TOTO");
        eqd.addFilterName("TATA");
        eqd.addFilterSource("JAVA");
        // Create dedicated 
        Assert.assertNotNull(cqb.cqlUserHitCount(eqd));
        Assert.assertNotNull(cqb.cqlCreateEvent(20));
    }
    
    /** TDD. */
    @Test
    public void testCassandraConnection() {
        Assert.assertNotNull(conn.getCluster());
        Assert.assertNotNull(conn.getKeySpace());
        Assert.assertNotNull(conn.getReplicationFactor());
        Assert.assertNull(conn.getUserName());
        conn.setKeySpace("KS1");
        conn.createKeySpace();
        conn.createKeySpace("KS1", 5);
        conn.setReplicationFactor(3);
        conn.dropSchema();
        conn.close();
    }
    
    @Test(expected = NoHostAvailableException.class)
    public void testCassandraConnectionEmpty() {
        Cluster cluster = Cluster.builder().addContactPoint("localhost").build();
        CassandraConnection cc = new CassandraConnection(cluster);
        cc.initSession();
    }
    
    @Test(expected = NoHostAvailableException.class)
    public void testCassandraConnectionAuth() {
        CassandraConnection cc = new CassandraConnection();
        cc.setUserName("sample");
        cc.initSession();
    }
    
    
    /** TDD. */
    @Test
    public void testCassandraNoHost() {
        CassandraConnection cc = new CassandraConnection();
        cc.setUserName(cc.getUserName());
        cc.setUserPassword(cc.getUserPassword());
        cc.setHostName(cc.getHostName());
        cc.setPort(cc.getPort());
        cc.setReplicationFactor(cc.getReplicationFactor());
        cc.setCluster(null);
        
        CassandraConnection c2 = new CassandraConnection("username", "password");
        Assert.assertNotNull(c2.getUserName());
    }
    
    /** TDD. */
    @Test(expected = IllegalArgumentException.class)
    public void testCassandraNoHost3() {
        Cluster ccc = null;
        new CassandraConnection(ccc);
    }
    
    /** Ff4j
    private static FF4j ff4j; 
    
    @BeforeClass
    public static void init() {
        CassandraConnection conn = new CassandraConnection();
        conn.createKeySpace();
        ff4j = new FF4j();
        ff4j.setFeatureStore(new FeatureStoreCassandra(conn));
        ff4j.setPropertiesStore(new PropertyStoreCassandra(conn));
        ff4j.setEventRepository(new EventRepositoryCassandra(conn));
        // Test Perspective
        //conn.dropSchema();
        ff4j.createSchema();
        ff4j.audit(true);
    }
    
    @Test
    public void  testCassandra() {
        /* Creation Feature
        Feature fx = new Feature("fx3", true, "sampleDesc", "group5", Util.set("admin", "user"));
        fx.setFlippingStrategy(new PonderationStrategy(0.5d));
        fx.addProperty(new PropertyString("p1", "v2"));
        fx.addProperty(new PropertyString("p2", "samples"));
        ff4j.createFeature(fx);
        
        /* Creation properties
        Property<Double> px = new PropertyDouble("d1", 1.25d);
        px.setDescription("Sample double");
        px.setFixedValues(Util.set(1.25d, 1.5d));
        ff4j.createProperty(px);
        
        // Event
        
        /* Read
        Feature fx1 = ff4j.getFeature("fx1");
        System.out.println(fx1.getFlippingStrategy().getInitParams());
        System.out.println(fx1.getCustomProperties().get("p1").getName());
        System.out.println(fx1.getPermissions().size());
        
        // ReadAll
        //System.out.println(ff4j.getFeatureStore().readAll());
        
        // Grant
        //ff4j.getFeatureStore().grantRoleOnFeature("fx3", "aaa");
        
        // Remove
        //ff4j.getFeatureStore().removeRoleFromFeature("fx3", "aaa");
        
        //ff4j.getFeatureStore().readAll();
        //ff4j.enable("fx3");
        //ff4j.check("fx3");
        //ff4j.check("fx2");
        
        long now = System.currentTimeMillis();
        EventQueryDefinition eqd = new EventQueryDefinition(now - 1000*24*3600, now);
        //System.out.println(ff4j.getEventRepository().getFeatureUsageHitCount(eqd));
        //System.out.println(ff4j.getEventRepository().getUserHitCount(eqd));
        //System.out.println(ff4j.getEventRepository().getSourceHitCount(eqd));
        //System.out.println(ff4j.getEventRepository().getHostHitCount(eqd));
        System.out.println(ff4j.getEventRepository().getFeatureUsageHistory(eqd, TimeUnit.HOURS));
    }*/
    
    
}
