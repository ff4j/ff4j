package org.ff4j.cli;

/*
 * #%L
 * ff4j-cli
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
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

/**
 * Test for command line interface.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class TestNotConnectedList {
	
	/** Expect to initialize with the processor. */
	private FF4jCliProcessor processor;
	
	/** Trap output. */
	private final ByteArrayOutputStream outContent = new ByteArrayOutputStream();
	
	/** Trap error. */
	private final ByteArrayOutputStream errContent = new ByteArrayOutputStream();
	
	/**
	 * Utility for testing output.
	 *
	 * @param expression
	 */
	private void assertOutputContains(String... expression) {
		String output = outContent.toString();
		if (expression != null) {
			for (String string : expression) {
				
				assertTrue("Output must contain :" + string, output.contains(string));
			}
		}
	}
	
	@Before
	public void init() {
		processor = new FF4jCliProcessor("ff4j-cli-config.xml");
		System.setOut(new PrintStream(outContent));
		System.setErr(new PrintStream(errContent));
	}
	
	@Test
	public void testNotConnectedList() {
		processor.evaluate("list");
		assertOutputContains("dev", "int");
	}
	
	@After
	public void cleanUpStreams() {
	    System.setOut(null);
	    System.setErr(null);
	}

}
