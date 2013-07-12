package org.ff4j.test;

import org.ff4j.FF4j;
import org.junit.Test;

public class GettingStartedCode {

	@Test
	public void myFirstTest() {

		// Default status disabled
		FF4j.createFeature("someFeature");
		
		if (FF4j.isFlipped("someFeature")) {
			System.out.println("Feature is UP !");
		} else {
			System.out.println("Feature is DOWN !");
		}
	}
	
	@Test
	public void mySecondTest() {

		FF4j.autoCreateFeature(true);
		if (FF4j.isFlipped("feature2")) {
			System.out.println("Feature is UP !");
		} else {
			System.out.println("Feature is DOWN !");
		}
	}
	
}

