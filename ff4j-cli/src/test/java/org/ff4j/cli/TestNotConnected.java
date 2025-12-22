package org.ff4j.cli;

/*-
 * #%L
 * ff4j-cli
 * %%
 * Copyright (C) 2013 - 2025 FF4J
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

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

/**
 * Test for command line interface.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
@Disabled
public class TestNotConnected extends AbstractCommandLineTest {
	
	@Test
	public void testCmd() {
		FF4jCliDisplay.displayBanner();
		FF4jCliDisplay.displayPrompt(null);
	}
	
	@Test
	public void testCmdHelp() {
		processor.evaluate("help");
		assertOutputContains("you are not connected");
	}
	
	@Test
	public void testCmdHelp2() {
	    System.out.println("testCmdHelp2");
	    System.out.println(processor);
		processor.evaluate("?");
		assertOutputContains("you are not connected");
	}

	@Test
	public void testCmdList() {
		processor.evaluate("list");
	}
	
	@Test
	public void testCmdls() {
		processor.evaluate("ls");
	}
	
	@Test
	public void testCmdInvalidWord() {
		processor.evaluate("invalidCommand");
	}
	
	@Test
	public void testCmdConnectInvalidEnv() {
		processor.evaluate("connect toto");
		Assertions.assertNull(processor.getCurrentEnv());
	}
	
	@Test
	public void testCmdConnectInvalidCredential() {
		processor.evaluate("connect dev -u invalid");
		Assertions.assertNull(processor.getCurrentEnv());

		processor.evaluate("connect dev -u invalid -p invalid");
		Assertions.assertNull(processor.getCurrentEnv());
		
		processor.evaluate("connect dev -u admin -p invalid");
		Assertions.assertNull(processor.getCurrentEnv());
	}
	
	@Test
	public void testCmdConnect() {
		processor.evaluate("connect dev -u admin -p admin");
		Assertions.assertEquals("dev", processor.getCurrentEnv());
	}

}
