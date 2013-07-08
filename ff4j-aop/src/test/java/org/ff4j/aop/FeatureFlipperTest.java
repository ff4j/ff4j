package org.ff4j.aop;

import junit.framework.Assert;

import org.ff4j.FF4j;
import org.ff4j.Feature;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;


@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("classpath:applicationContext-ff4j-aop-test.xml")
public class FeatureFlipperTest {

	@Autowired
	@Qualifier("greeting.english")
	private GreetingService greeting;

	@Test
	public void testAnnotatedFlipping() {
		FF4j.createFeature(new Feature("language-french", false));
		Assert.assertTrue(greeting.sayHello("CLU").startsWith("Hello"));
		FF4j.enableFeature("language-french");
		Assert.assertTrue("Service did not flipped", greeting.sayHello("CLU").startsWith("Bonjour"));
	}
}