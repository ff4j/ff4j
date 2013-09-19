package org.ff4j.test.store;

import junit.framework.Assert;

import org.ff4j.FF4j;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations={"classpath:*applicationContext-core-test.xml"})
public class CoreTestSpring {

	@Test
	public void testWithSpring() {
		Assert.assertTrue(FF4j.sIsFlipped("first"));
	}
}

