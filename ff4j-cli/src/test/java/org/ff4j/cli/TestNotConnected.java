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

import org.junit.Assert;
import org.junit.Test;

/**
 * Test for command line interface.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
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
		Assert.assertNull(processor.getCurrentEnv());
	}
	
	@Test
	public void testCmdConnectInvalidCredential() {
		processor.evaluate("connect dev -u invalid");
		Assert.assertNull(processor.getCurrentEnv());

		processor.evaluate("connect dev -u invalid -p invalid");
		Assert.assertNull(processor.getCurrentEnv());
		
		processor.evaluate("connect dev -u admin -p invalid");
		Assert.assertNull(processor.getCurrentEnv());
	}
	
	@Test
	public void testCmdConnect() {
		processor.evaluate("connect dev -u admin -p admin");
		Assert.assertEquals("dev", processor.getCurrentEnv());
	}

}
