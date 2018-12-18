package org.ff4j.event;

import java.util.ArrayList;
import java.util.List;

import org.ff4j.FF4j;
import org.ff4j.event.repository.EventFeatureUsageRepository;
import org.ff4j.event.repository.EventFeatureUsageRepositorySupport;
import org.ff4j.parser.ConfigurationFileParser;
import org.ff4j.parser.FF4jConfigFile;
import org.ff4j.test.AssertFF4j;
import org.ff4j.test.FF4jTestDataSet;
import org.ff4j.utils.Util;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

/**
 * Superclass to test {@link EventFeatureUsageRepository}.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public abstract class EventRepositoryTest implements FF4jTestDataSet {
   
    /** Initialize */
    protected FF4j ff4j = null;
    
    /** Tested Store. */
    protected EventFeatureUsageRepository testedStore;

    /** Test Values */
    protected AssertFF4j assertFF4j;
    
    /** DataSet. **/
    protected FF4jConfigFile testDataSet;
    
    /** {@inheritDoc} */
    @BeforeEach
    public void setUp() throws Exception {
        ConfigurationFileParser.clearCache();
        ff4j        = new FF4j().withRepositoryEventFeaturesUsage(initRepository());
        assertFF4j  = new AssertFF4j(ff4j);
        testedStore = ff4j.getRepositoryEventFeaturesUsage();
        testDataSet = expectConfig();
    }
   
    // I need listener cannot get EventFeatureUsageRepository only.
    protected abstract EventFeatureUsageRepositorySupport initRepository();
    
    // Utility to generate event
    protected Event generateFeatureUsageEvent(String uid) {
        return new Event().source(Event.Source.JAVA_API)
                .targetUid(uid)
                .scope(Event.Scope.FEATURE)
                .action(Event.Action.HIT);
    }
    
    // Generate a random event during the period
    protected Event generateFeatureUsageEvent(String uid, long timestamp) {
        Event event = generateFeatureUsageEvent(uid);
        event.setTimestamp(timestamp);
        return event;
    }
    
    // Generate a random event during the period
    protected Event generateFeatureUsageEvent(String uid, long from, long to) {
        return generateFeatureUsageEvent(uid, from + (long) (Math.random() * (to-from)));
    }
    
    List <String > featureNames = new ArrayList<>();
    
    protected String getRandomFeatureName() {
        if (featureNames == null) {
            featureNames = new ArrayList<>(testDataSet.getFeatures().keySet());
        }
        return Util.getRandomElement(featureNames);
    }
    
    // Generate a random event during the period
    protected Event generateRandomFeatureUsageEvent(long from, long to) {
        return generateFeatureUsageEvent(getRandomFeatureName(), from , to);
    }
    
    // Populate repository for test
    protected void populateRepository(long from, long to, int totalEvent) throws InterruptedException {
        for (int i = 0; i < totalEvent; i++) {
            testedStore.save(generateRandomFeatureUsageEvent(from, to));
        }
    }
    
    @Test
    @DisplayName("When using a feature a hitcount is record in the repository")
    public void testSaveEventUnit() throws InterruptedException {
        long start = System.currentTimeMillis();
        EventQueryDefinition query = new EventQueryDefinition(start, System.currentTimeMillis());
        Assertions.assertEquals(0, testedStore.getTotalHitCount(query));
        testedStore.save(generateFeatureUsageEvent("f1"));
        EventQueryDefinition queryfewMillis = new EventQueryDefinition(start-20, System.currentTimeMillis());
        Thread.sleep(100);
        Assertions.assertEquals(1, testedStore.getTotalHitCount(queryfewMillis));
    }
    
    /*
    @Test(expected = IllegalArgumentException.class)
    public void testSaveEventNull() {
        Assert.assertFalse(repo.saveEvent(null));
    }
    
    @Test
    public void testSaveAuditTrail() throws InterruptedException {
        long start = System.currentTimeMillis();
        Event evt1 = new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", EventConstants.ACTION_CREATE);
        Assert.assertTrue(repo.saveEvent(evt1));
        Thread.sleep(200);
        Assert.assertEquals(1, repo.getAuditTrail(new EventQueryDefinition(start-10, System.currentTimeMillis())).size());
    }
    
    @Test
    public void testPieChart() throws InterruptedException {
        long start = System.currentTimeMillis();
        Event evt1 = new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", EventConstants.ACTION_CREATE);
        Assert.assertTrue(repo.saveEvent(evt1));
        Thread.sleep(200);
        
        EventQueryDefinition eqd = new EventQueryDefinition(start-10, System.currentTimeMillis());
        Assert.assertNotNull(repo.getFeatureUsagePieChart(eqd));
        Assert.assertNotNull(repo.getHostPieChart(eqd));
        Assert.assertNotNull(repo.getSourcePieChart(eqd));
        Assert.assertNotNull(repo.getUserPieChart(eqd));
        
        Assert.assertNotNull(repo.getHostBarChart(eqd));
        Assert.assertNotNull(repo.getSourceBarChart(eqd));
        Assert.assertNotNull(repo.getUserBarChart(eqd));
        
    }
    
    @Test
    public void testFeatureUsageBarCharts() throws InterruptedException {
        long start = System.currentTimeMillis();
        // Create Event
        repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", EventConstants.ACTION_CREATE));
        for(int i = 0;i<8;i++) {
            Thread.sleep(100);
            repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CHECK_OK));
            repo.saveEvent(new Event(SOURCE_WEB, TARGET_FEATURE,  "f2", ACTION_CHECK_OK));
        }
        
        // Assert bar chart (2 bars with 8 and 8)
        EventQueryDefinition testQuery = new EventQueryDefinition(start-10, System.currentTimeMillis()+10);
        BarChart bChart = repo.getFeatureUsageBarChart(testQuery);
        Assert.assertEquals(2, bChart.getChartBars().size());
        Assert.assertEquals(new Integer(8), bChart.getChartBars().get(0).getValue());
        Assert.assertEquals(new Integer(8), bChart.getChartBars().get(1).getValue());
        Assert.assertNotNull(bChart.getChartBars().get(0).getColor());
        Assert.assertNotNull(bChart.getChartBars().get(1).getColor());
    }
    
    @Test
    public void testFeatureUsageHitCount() throws InterruptedException {
        long start = System.currentTimeMillis();
        // Create Event
        repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", EventConstants.ACTION_CREATE));
        for(int i = 0;i<8;i++) {
            Thread.sleep(100);
            repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", EventConstants.ACTION_CHECK_OK));
            repo.saveEvent(new Event(SOURCE_WEB, TARGET_FEATURE, "f2", EventConstants.ACTION_CHECK_OK));
        }
        Thread.sleep(100);
        
        // Assert bar chart (2 bars with 8 and 8)
        EventQueryDefinition testQuery = new EventQueryDefinition(start, System.currentTimeMillis());
        // Assert Pie Chart (2 sectors with 8 and 8)
        Map < String, MutableHitCount > mapOfHit = repo.getFeatureUsageHitCount(testQuery);
        Assert.assertEquals(2, mapOfHit.size());
        Assert.assertTrue(mapOfHit.containsKey("f1"));
        Assert.assertTrue(mapOfHit.containsKey("f2"));
        Assert.assertEquals(8, mapOfHit.get("f1").get());
    }
    
    @Test
    public void testSearchFeatureUsageEvents() throws InterruptedException {
        long start = System.currentTimeMillis();
        repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CREATE));
        for(int i = 0;i<8;i++) {
            Thread.sleep(100);
            repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CHECK_OK));
            repo.saveEvent(new Event(SOURCE_WEB, TARGET_FEATURE, "f2", ACTION_CHECK_OK));
        }
        Thread.sleep(100);
        
        // Then
        EventQueryDefinition testQuery = new EventQueryDefinition(start-20, System.currentTimeMillis());
        EventSeries es = repo.searchFeatureUsageEvents(testQuery);
        Assert.assertEquals(16, es.size());
        
        // Then
        
    }
    
    @Test
    public void testGetFeatureUsageHistory() throws InterruptedException {
        long start = System.currentTimeMillis();
        repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CREATE));
        for(int i = 0;i<8;i++) {
            Thread.sleep(100);
            repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CHECK_OK));
            repo.saveEvent(new Event(SOURCE_WEB, TARGET_FEATURE, "f2", ACTION_CHECK_OK));
        }
        Thread.sleep(100);
        
        // Then
        EventQueryDefinition testQuery = new EventQueryDefinition(start-20, System.currentTimeMillis());
        TimeSeriesChart  tsc = repo.getFeatureUsageHistory(testQuery, TimeUnit.HOURS);
        Assert.assertEquals(1, tsc.getTimeSlots().size());
    }
    
    /** TDD. *
    @Test
    public void testSourceHitCount() throws InterruptedException {
        long start = System.currentTimeMillis();
        // When
        for(int i = 0;i<8;i++) {
            Thread.sleep(100);
            repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CHECK_OK));
            repo.saveEvent(new Event(SOURCE_WEB,  TARGET_FEATURE, "f2", ACTION_CHECK_OK));
        }
        Thread.sleep(200);
        repo.saveEvent(new Event(SOURCE_WEBAPI, TARGET_FEATURE, "f1", ACTION_CHECK_OK));
        Thread.sleep(200);
        
        // Then
        EventQueryDefinition testQuery = new EventQueryDefinition(start-20, System.currentTimeMillis());
        Map < String, MutableHitCount > mapOfHit = repo.getSourceHitCount(testQuery);
        Assert.assertEquals(3, mapOfHit.size());
        Assert.assertTrue(mapOfHit.containsKey(SOURCE_JAVA));
        Assert.assertTrue(mapOfHit.containsKey(SOURCE_WEB));
        Assert.assertEquals(1, mapOfHit.get(SOURCE_WEBAPI).get());
    }
    
    /** TDD. *
    @Test
    public void testUserHitCount() throws InterruptedException {
        long start = System.currentTimeMillis();
        // When
        for(int i = 0;i<8;i++) {
            Event e1 = new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CHECK_OK);
            e1.setUser("JOHN");
            repo.saveEvent(e1);
            Thread.sleep(100);
            
            Event e2 = new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CHECK_OK);
            e2.setUser("BOB");
            repo.saveEvent(e2);
            Thread.sleep(100);
        }
        Thread.sleep(200);
        
        // Then
        EventQueryDefinition testQuery = new EventQueryDefinition(start-20, System.currentTimeMillis());
        Map < String, MutableHitCount > mapOfHit = repo.getUserHitCount(testQuery);
        Assert.assertEquals(2, mapOfHit.size());
        Assert.assertTrue(mapOfHit.containsKey("JOHN"));
        Assert.assertTrue(mapOfHit.containsKey("BOB"));
        Assert.assertEquals(8, mapOfHit.get("BOB").get());
    }
    
    /** TDD. *
    @Test
    public void testHostHitCount() throws InterruptedException {
        long start = System.currentTimeMillis();
        // When
        for(int i = 0;i<8;i++) {
            Thread.sleep(100);
            repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CHECK_OK));
        }
        Thread.sleep(200);
        
        // Then
        EventQueryDefinition testQuery = new EventQueryDefinition(start, System.currentTimeMillis());
        Map < String, MutableHitCount > mapOfHit = repo.getHostHitCount(testQuery);
        Assert.assertEquals(1, mapOfHit.size());
        Assert.assertEquals(1, mapOfHit.values().size());
    }
    
    /** TDD. *
    @Test
    public void testSaveCheckOff() throws InterruptedException {
        long start = System.currentTimeMillis();
        // Given
        Event evt1 = new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CHECK_OFF);
        // When
        Assert.assertTrue(repo.saveEvent(evt1));
        Thread.sleep(100);
        // Then
        Assert.assertEquals(0, repo.getFeatureUsageTotalHitCount(new EventQueryDefinition(start, System.currentTimeMillis())));
        Assert.assertEquals(0, repo.getAuditTrail(new EventQueryDefinition(start, System.currentTimeMillis())).size());
    }
    
    /** TDD. *
    @Test
    public void testLimitEventSeries() throws InterruptedException {
        EventSeries es = new EventSeries(5);
        for(int i=0;i<10;i++) {
            Thread.sleep(10);
            es.add(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CREATE));
        }
        Assert.assertEquals(5, es.size());
    }
    
    /** TDD. *
    @Test
    public void testGetEventByUID() throws InterruptedException {
        // Given
        String dummyId = "1234-5678-9012-3456";
        Event evt1 = new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CREATE);
        evt1.setUuid(dummyId);
        // When
        repo.saveEvent(evt1);
        // Let the store to be updated
        Thread.sleep(100);
        // Then
        Event evt = repo.getEventByUUID(dummyId, System.currentTimeMillis());
        Assert.assertNotNull(evt);
    }
    
    /** TDD. *
    @Test
    public void testGetEventByUID2() throws InterruptedException {
        // Given
        String dummyId = "1234-5678-9012-3456";
        Event evt1 = new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CREATE);
        evt1.setUuid(dummyId);
        // When
        repo.saveEvent(evt1);
        // Let the store to be updated
        Thread.sleep(100);
        // Then
        Event evt = repo.getEventByUUID(dummyId, null);
        Assert.assertNotNull(evt);
    }
    
    /** TDD. *
    @Test
    public void testPurgeEvents() throws InterruptedException {
        // Given, 2 events in the repo
        long topStart = System.currentTimeMillis();
        Event evtAudit = new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CREATE);
        evtAudit.setUuid("1234-5678-9012-3456");
        Event evtFeatureUsage = new Event(SOURCE_JAVA, TARGET_FEATURE, "f2", ACTION_CHECK_OK);
        evtFeatureUsage.setUuid("1234-5678-9012-3457");
        repo.saveEvent(evtAudit);
        repo.saveEvent(evtFeatureUsage);
        Thread.sleep(100);
        Assert.assertNotNull(repo.getEventByUUID(evtAudit.getUuid(), System.currentTimeMillis()));
        Assert.assertNotNull(repo.getEventByUUID(evtFeatureUsage.getUuid(), System.currentTimeMillis()));
        // When
        EventQueryDefinition testQuery = new EventQueryDefinition(topStart-100, System.currentTimeMillis());
        repo.purgeFeatureUsage(testQuery);
        Assert.assertNull(repo.getEventByUUID(evtFeatureUsage.getUuid(), System.currentTimeMillis()));
        Assert.assertTrue(repo.searchFeatureUsageEvents(testQuery).isEmpty());
        
        // Then
        EventQueryDefinition testQuery2 = new EventQueryDefinition(topStart-100, System.currentTimeMillis());
        repo.purgeAuditTrail(testQuery2);
        Assert.assertNull(repo.getEventByUUID(evtAudit.getUuid(), System.currentTimeMillis()));

    }*/

}

