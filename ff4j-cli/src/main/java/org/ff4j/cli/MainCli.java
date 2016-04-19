package org.ff4j.cli;

import static org.ff4j.cli.ansi.AnsiTerminal.logError;

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
import static org.ff4j.cli.ansi.AnsiTerminal.logWarn;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ff4j.FF4j;
import org.ff4j.cli.ansi.AnsiForegroundColor;
import org.ff4j.cli.ansi.AnsiTerminal;
import org.springframework.context.support.ClassPathXmlApplicationContext;

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
		FF4jCliDisplay.displayBanner();
		FF4jCliProcessor main = new FF4jCliProcessor("ff4j-cli-config.xml");
		logInfo("Enter '?' or 'help' to get help");
		BufferedReader console = new BufferedReader(new InputStreamReader(System.in));
		while (true) {
			FF4jCliDisplay.displayPrompt(main.getCurrentEnv());
			main.processCommandLine(console.readLine());
		}
	}
	
	
	
	
	

}
