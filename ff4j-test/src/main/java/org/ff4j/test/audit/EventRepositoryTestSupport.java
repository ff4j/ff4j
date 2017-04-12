package org.ff4j.test.audit;

import static org.ff4j.audit.EventConstants.ACTION_CHECK_OFF;
import static org.ff4j.audit.EventConstants.ACTION_CHECK_OK;
import static org.ff4j.audit.EventConstants.ACTION_CREATE;
import static org.ff4j.audit.EventConstants.SOURCE_JAVA;
import static org.ff4j.audit.EventConstants.SOURCE_WEB;
import static org.ff4j.audit.EventConstants.SOURCE_WEBAPI;
import static org.ff4j.audit.EventConstants.TARGET_FEATURE;

import java.util.ArrayList;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/*
 * #%L
 * ff4j-test
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

import org.ff4j.FF4j;
import org.ff4j.audit.Event;
import org.ff4j.audit.EventConstants;
import org.ff4j.audit.EventPublisher;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.EventSeries;
import org.ff4j.audit.MutableHitCount;
import org.ff4j.audit.chart.BarChart;
import org.ff4j.audit.chart.TimeSeriesChart;
import org.ff4j.audit.repository.EventRepository;
import org.ff4j.core.Feature;
import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.utils.Util;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

/**
 * Event Repository test support.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public abstract class EventRepositoryTestSupport {

	/** Initialize */
	protected FF4j ff4j = null;

	/** Feature List. */
	protected ArrayList<Feature> features;

	/** Target {@link EventRepository}. */
	protected EventRepository repo;

	/** Target publisher. */
	protected EventPublisher publisher;

	/** {@inheritDoc} */
	@Before
	public void setUp() throws Exception {
		ff4j = new FF4j();
		ff4j.setFeatureStore(new InMemoryFeatureStore("test-ff4j-features.xml"));
		ff4j.setPropertiesStore(new InMemoryPropertyStore("test-ff4j-features.xml"));
		ff4j.setEventRepository(initRepository());
		repo = ff4j.getEventRepository();
	}

	/**
	 * Event generation.
	 * 
	 * @param uid
	 *            target unique identifier
	 * @return unique event
	 */
	protected Event generateFeatureUsageEvent(String uid) {
		return new Event(SOURCE_JAVA, TARGET_FEATURE, uid, ACTION_CHECK_OK);
	}

	/**
	 * Event generation.
	 *
	 * @param uid
	 *            target unique identifier
	 * @param timestamp
	 *            current event time
	 * @return
	 */
	protected Event generateFeatureUsageEvent(String uid, long timestamp) {
		Event event = generateFeatureUsageEvent(uid);
		event.setTimestamp(timestamp);
		return event;
	}

	/**
	 * Generate random event.
	 *
	 * @param uid
	 *            event uid
	 * @param from
	 *            time slot in
	 * @param to
	 *            time slot to
	 * @return target event
	 */
	protected Event generateRandomFeatureUsageEvent(String uid, long from, long to) {
		return generateFeatureUsageEvent(uid, from + (long) (Math.random() * (to - from)));
	}

	/**
	 * Generate random event.
	 *
	 * @param uid
	 *            event uid
	 * @param from
	 *            time slot in
	 * @param to
	 *            time slot to
	 * @return target event
	 */
	protected Event generateRandomFeatureUsageEvent(long from, long to) {
		return generateRandomFeatureUsageEvent(Util.getRandomElement(features).getUid(), from, to);
	}

	/**
	 * Generate random events for.
	 */
	protected void populateRepository(long from, long to, int totalEvent) throws InterruptedException {
		for (int i = 0; i < totalEvent; i++) {
			repo.saveEvent(generateRandomFeatureUsageEvent(from, to));
		}
	}

	/**
	 * Any store test will declare its store through this callback.
	 * 
	 * @return working feature store
	 * @throws Exception
	 *             error during building feature store
	 */
	protected abstract EventRepository initRepository();

	/** TDD. */
	@Test
	public void testSaveEventUnit() throws InterruptedException {
		long start = System.currentTimeMillis();
		// Given
		EventQueryDefinition eqd = new EventQueryDefinition(start, System.currentTimeMillis());
		Assert.assertEquals(0, repo.getFeatureUsageTotalHitCount(eqd));
		// When
		repo.saveEvent(generateFeatureUsageEvent("f1"));
		// Wait for the event to be effectively store
		Thread.sleep(100);
		// Then
		EventQueryDefinition eqd2 = new EventQueryDefinition(start - 20, System.currentTimeMillis());
		Assert.assertEquals(1, repo.getFeatureUsageTotalHitCount(eqd2));
	}

	/** TDD. */
	@Test(expected = IllegalArgumentException.class)
	public void testSaveEventNull() {
		Assert.assertFalse(repo.saveEvent(null));
	}

	/** TDD. */
	@Test
	public void testSaveAuditTrail() throws InterruptedException {
		long start = System.currentTimeMillis();
		// Given
		Event evt1 = new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", EventConstants.ACTION_CREATE);
		// When
		repo.saveEvent(evt1);
		// Wait for the event to be effectively store
		Thread.sleep(200);
		EventQueryDefinition eqd2 = new EventQueryDefinition(start - 200, System.currentTimeMillis());
		Assert.assertEquals(1, repo.getAuditTrail(eqd2).size());
	}

	/** TDD. */
	@Test
	public void testFeatureUsageBarCharts() throws InterruptedException {
		long start = System.currentTimeMillis();
		// Given empty event repository
		// When
		repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", EventConstants.ACTION_CREATE));
		for (int i = 0; i < 8; i++) {
			Thread.sleep(100);
			repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CHECK_OK));
			repo.saveEvent(new Event(SOURCE_WEB, TARGET_FEATURE, "f2", ACTION_CHECK_OK));
		}
		// Then : Assert bar chart (2 bars with 8 and 8)
		EventQueryDefinition testQuery = new EventQueryDefinition(start - 10, System.currentTimeMillis() + 10);
		BarChart bChart = repo.getFeatureUsageBarChart(testQuery);

		Assert.assertEquals(2, bChart.getChartBars().size());
		Assert.assertEquals(new Integer(8), bChart.getChartBars().get(0).getValue());
		Assert.assertEquals(new Integer(8), bChart.getChartBars().get(1).getValue());
		Assert.assertNotNull(bChart.getChartBars().get(0).getColor());
		Assert.assertNotNull(bChart.getChartBars().get(1).getColor());
	}

	/** TDD. */
	@Test
	public void testFeatureUsageHitCount() throws InterruptedException {
		long start = System.currentTimeMillis();
		// Given empty event repository
		// When
		repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CREATE));
		for (int i = 0; i < 8; i++) {
			Thread.sleep(100);
			repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CHECK_OK));
			repo.saveEvent(new Event(SOURCE_WEB, TARGET_FEATURE, "f2", ACTION_CHECK_OK));
		}
		Thread.sleep(100);

		// Then
		EventQueryDefinition testQuery = new EventQueryDefinition(start, System.currentTimeMillis());
		// Assert Pie Chart (2 sectors with 8 and 8)
		Map<String, MutableHitCount> mapOfHit = repo.getFeatureUsageHitCount(testQuery);
		Assert.assertEquals(2, mapOfHit.size());
		Assert.assertTrue(mapOfHit.containsKey("f1"));
		Assert.assertTrue(mapOfHit.containsKey("f2"));
		Assert.assertEquals(8, mapOfHit.get("f1").get());
	}

	@Test
	public void testSearchFeatureUsageEvents() throws InterruptedException {
		long start = System.currentTimeMillis();
		repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CREATE));
		for (int i = 0; i < 8; i++) {
			Thread.sleep(100);
			repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CHECK_OK));
			repo.saveEvent(new Event(SOURCE_WEB, TARGET_FEATURE, "f2", ACTION_CHECK_OK));
		}
		Thread.sleep(100);

		// Then
		EventQueryDefinition testQuery = new EventQueryDefinition(start - 20, System.currentTimeMillis());
		EventSeries es = repo.searchFeatureUsageEvents(testQuery);
		Assert.assertEquals(16, es.size());

		// Then

	}

	@Test
	public void testGetFeatureUsageHistory() throws InterruptedException {
		long start = System.currentTimeMillis();
		repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CREATE));
		for (int i = 0; i < 8; i++) {
			Thread.sleep(100);
			repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CHECK_OK));
			repo.saveEvent(new Event(SOURCE_WEB, TARGET_FEATURE, "f2", ACTION_CHECK_OK));
		}
		Thread.sleep(100);

		// Then
		EventQueryDefinition testQuery = new EventQueryDefinition(start - 20, System.currentTimeMillis());
		TimeSeriesChart tsc = repo.getFeatureUsageHistory(testQuery, TimeUnit.HOURS);
		Assert.assertEquals(1, tsc.getTimeSlots().size());
	}

	/** TDD. */
	@Test
	public void testSourceHitCount() throws InterruptedException {
		long start = System.currentTimeMillis();
		// When
		for (int i = 0; i < 8; i++) {
			Thread.sleep(100);
			repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CHECK_OK));
			repo.saveEvent(new Event(SOURCE_WEB, TARGET_FEATURE, "f2", ACTION_CHECK_OK));
		}
		Thread.sleep(200);
		repo.saveEvent(new Event(SOURCE_WEBAPI, TARGET_FEATURE, "f1", ACTION_CHECK_OK));
		Thread.sleep(200);

		// Then
		EventQueryDefinition testQuery = new EventQueryDefinition(start - 20, System.currentTimeMillis());
		Map<String, MutableHitCount> mapOfHit = repo.getSourceHitCount(testQuery);
		Assert.assertEquals(3, mapOfHit.size());
		Assert.assertTrue(mapOfHit.containsKey(SOURCE_JAVA));
		Assert.assertTrue(mapOfHit.containsKey(SOURCE_WEB));
		Assert.assertEquals(1, mapOfHit.get(SOURCE_WEBAPI).get());
	}

	/** TDD. */
	@Test
	public void testUserHitCount() throws InterruptedException {
		long start = System.currentTimeMillis();
		// When
		for (int i = 0; i < 8; i++) {
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
		EventQueryDefinition testQuery = new EventQueryDefinition(start - 20, System.currentTimeMillis());
		Map<String, MutableHitCount> mapOfHit = repo.getUserHitCount(testQuery);
		Assert.assertEquals(2, mapOfHit.size());
		Assert.assertTrue(mapOfHit.containsKey("JOHN"));
		Assert.assertTrue(mapOfHit.containsKey("BOB"));
		Assert.assertEquals(8, mapOfHit.get("BOB").get());
	}

	/** TDD. */
	@Test
	public void testHostHitCount() throws InterruptedException {
		long start = System.currentTimeMillis();
		// When
		for (int i = 0; i < 8; i++) {
			Thread.sleep(100);
			repo.saveEvent(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CHECK_OK));
		}
		Thread.sleep(200);

		// Then
		EventQueryDefinition testQuery = new EventQueryDefinition(start, System.currentTimeMillis());
		Map<String, MutableHitCount> mapOfHit = repo.getHostHitCount(testQuery);
		Assert.assertEquals(1, mapOfHit.size());
		Assert.assertEquals(1, mapOfHit.values().size());
	}

	/** TDD. */
	@Test
	public void testSaveCheckOff() throws InterruptedException {
		long start = System.currentTimeMillis();
		// Given
		Event evt1 = new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CHECK_OFF);
		// When
		Assert.assertTrue(repo.saveEvent(evt1));
		Thread.sleep(100);
		// Then
		Assert.assertEquals(0,
				repo.getFeatureUsageTotalHitCount(new EventQueryDefinition(start, System.currentTimeMillis())));
		Assert.assertEquals(0, repo.getAuditTrail(new EventQueryDefinition(start, System.currentTimeMillis())).size());
	}

	/** TDD. */
	@Test
	public void testLimitEventSeries() throws InterruptedException {
		EventSeries es = new EventSeries(5);
		for (int i = 0; i < 10; i++) {
			Thread.sleep(10);
			es.add(new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CREATE));
		}
		Assert.assertEquals(5, es.size());
	}

	/** TDD. */
	@Test
	public void testGetEventByUID() throws InterruptedException {
		// Given
		String dummyId = "1234-5678-9012-3456";
		Event evt1 = new Event(SOURCE_JAVA, TARGET_FEATURE, "f1", ACTION_CHECK_OFF);
		evt1.setUuid(dummyId);
		// When
		repo.saveEvent(evt1);
		// Let the store to be updated
		Thread.sleep(100);
		// Then
		Event evt = repo.getEventByUUID(dummyId, System.currentTimeMillis());
		Assert.assertNotNull(evt);
	}

	/** TDD. */
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
		EventQueryDefinition testQuery = new EventQueryDefinition(topStart - 100, System.currentTimeMillis());
		repo.purgeAuditTrail(testQuery);

		// Then
		Assert.assertNull(repo.getEventByUUID(evtAudit.getUuid(), System.currentTimeMillis()));
		repo.purgeFeatureUsage(testQuery);
		Thread.sleep(100);
		Assert.assertNull(repo.getEventByUUID(evtFeatureUsage.getUuid(), System.currentTimeMillis()));
	}

}
