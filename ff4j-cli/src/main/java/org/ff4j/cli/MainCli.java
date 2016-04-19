package org.ff4j.cli;

import static org.ff4j.cli.FF4jCliDisplay.displayBanner;
import static org.ff4j.cli.FF4jCliDisplay.displayPrompt;

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

import static org.ff4j.cli.ansi.AnsiTerminal.logInfo;

import java.io.BufferedReader;
import java.io.InputStreamReader;

/**
 * Command line to operate FF4J.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class MainCli {
	
	/**
	 * Main loop.
	 *
	 * @param args
	 *            all parameters related to ff4j commands
	 * @throws Exception
	 *             error during manipulation
	 */
	public static void main(String[] args) throws Exception {
		// TODO if -conf option define other location for configuration file
		
		/* TODO 
		 * Bridge operations from ff4j to main 
		 * ff4j -env dev -u admin -p admin -op enableFeature -f f1
		 */
		
		displayBanner();
		
		FF4jCliProcessor main = new FF4jCliProcessor("ff4j-cli-config.xml");
		logInfo("Enter '?' or 'help' to get help");
		
		// Connect to System.in
		BufferedReader console = new BufferedReader(new InputStreamReader(System.in));
		while (true) {
			displayPrompt(main.getCurrentEnv());
			main.evaluate(console.readLine());
		}
		
	}
}
