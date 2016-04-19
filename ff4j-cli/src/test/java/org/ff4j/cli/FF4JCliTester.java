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


import static org.junit.Assert.*;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

/**
 * Test for command line interface.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
@Ignore
public class FF4JCliTester {
	
	/** Expect to initialize with the processor. */
	private FF4jCliProcessor processor;
	
	/** Trap output. */
	private final ByteArrayOutputStream outContent = new ByteArrayOutputStream();
	
	/** Trap error. */
	private final ByteArrayOutputStream errContent = new ByteArrayOutputStream();
			
	@Before
	public void processor() {
		processor = new FF4jCliProcessor("ff4j-cli-config.xml");
		System.setOut(new PrintStream(outContent));
		System.setErr(new PrintStream(errContent));
	}
	
	@Test
	public void testHelp() {
		processor.evalue("help");
		assertEquals("hello", outContent.toString());
	}
	
	@After
	public void cleanUpStreams() {
	    System.setOut(null);
	    System.setErr(null);
	}

}
