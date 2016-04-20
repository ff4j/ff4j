package org.ff4j.cli;

import static org.junit.Assert.assertTrue;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

import org.junit.After;
import org.junit.Before;

public abstract class AbstractCommandLineTest {

	/** Expect to initialize with the processor. */
	protected FF4jCliProcessor processor;
	
	/** Trap output. */
	protected final ByteArrayOutputStream outContent = new ByteArrayOutputStream();
	
	/** Trap error. */
	protected final ByteArrayOutputStream errContent = new ByteArrayOutputStream();
	
	/**
	 * Utility for testing output.
	 *
	 * @param expression
	 */
	protected void assertOutputContains(String... expression) {
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
	
	@After
	public void cleanUpStreams() {
	    System.setOut(null);
	    System.setErr(null);
	}
	
	
		
}
