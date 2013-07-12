package org.ff4j.test;

import static org.ff4j.FF4j.disableFeature;
import static org.ff4j.FF4j.isFlipped;

import org.junit.Test;

public class GettingStartedCode01 {
	
	@Test
	public void helloTest() {
		
		if (isFlipped("first")) {
			System.out.println("Hello World !! Yes it Works !");
		}
		
		disableFeature("first");
		
		if (!isFlipped("first")) {
			System.out.println("Back to reality !!");
		}
	}
}
