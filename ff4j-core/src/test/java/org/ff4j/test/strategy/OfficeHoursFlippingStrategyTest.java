package org.ff4j.test.strategy;

import org.ff4j.FF4j;
import org.junit.Test;

public class OfficeHoursFlippingStrategyTest  {
	
	@Test
	public void testExpression() throws Exception {
		
		new FF4j("ff4j-office.xml");
		//Assert.assertTrue(FF4j.isFlipped("displayCallMeButton"));
		
	}
		
}
