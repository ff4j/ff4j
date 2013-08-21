package org.ff4j.aop;

import junit.framework.Assert;

import org.ff4j.FF4j;
import org.junit.After;
import org.junit.BeforeClass;
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

	@Autowired
	@Qualifier("goodbye.french")
	private GoodbyeService goodbye;

	@BeforeClass
	public static void createFeatures() {
		FF4j.createFeature("language-french");
		FF4j.createFeature("language-english");
	}

	@After
	public void disableFeatures() {
		FF4j.disableFeature("language-french");
		FF4j.disableFeature("language-english");
	}

	@Test
	public void testAnnotatedFlipping_with_alterBean() {
		Assert.assertTrue(greeting.sayHello("CLU").startsWith("Hello"));

		FF4j.enableFeature("language-french");
		Assert.assertTrue("Service did not flipped", greeting.sayHello("CLU").startsWith("Bonjour"));
	}

	@Test
	public void testAnnotatedFlipping_with_alterClazz() {
		Assert.assertTrue(greeting.sayHelloWithClass("CLU").startsWith("Hi"));

		FF4j.enableFeature("language-french");
		Assert.assertTrue("Service did not flipped", greeting.sayHelloWithClass("CLU").startsWith("Salut"));
	}

	@Test
	public void testAnnotatedFlipping_if_qualified_implementation_is_not_the_first_class_qualified_name_in_natural_ordering() {
		Assert.assertTrue(goodbye.sayGoodbye("CLU").startsWith("Au revoir"));

		FF4j.enableFeature("language-english");
		Assert.assertTrue("Service did not flipped", goodbye.sayGoodbye("CLU").startsWith("Goodbye"));
	}

	@Test
	public void testAnnotatedFlipping_with_alterClazz_if_qualified_implementation_is_not_the_first_class_qualified_name_in_natural_ordering() {
		Assert.assertTrue(goodbye.sayGoodbyeWithClass("CLU").startsWith("A plus"));

		FF4j.enableFeature("language-english");
		Assert.assertTrue("Service did not flipped", goodbye.sayGoodbyeWithClass("CLU").startsWith("See you"));
	}
}