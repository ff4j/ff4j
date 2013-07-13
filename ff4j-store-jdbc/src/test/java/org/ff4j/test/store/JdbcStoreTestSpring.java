package org.ff4j.test.store;

import java.io.IOException;
import java.io.InputStream;

import junit.framework.Assert;

import org.ff4j.FF4j;
import org.ff4j.store.FeatureLoader;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(locations={"classpath:*applicationContext-jdbc-test.xml"})
public class JdbcStoreTestSpring {

	@Test
	public void testWithSpring() throws IOException {
		Assert.assertTrue(FF4j.isFlipped("first"));
		
		
	}
}

