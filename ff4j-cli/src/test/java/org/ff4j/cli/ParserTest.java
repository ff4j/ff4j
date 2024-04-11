package org.ff4j.cli;

/*-
 * #%L
 * ff4j-cli
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import org.apache.commons.cli.CommandLine;

import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.ParseException;
import org.junit.Test;

public class ParserTest {

	@Test
	public void testParse() throws ParseException {

		HelpFormatter formatter = new HelpFormatter();
		formatter.printHelp("connect <envName> \nUse to connect to env\n", FF4jCliOptions.connectOptions());
		CommandLineParser parser = new DefaultParser();
		CommandLine cmd = parser.parse(FF4jCliOptions.connectOptions(),
				new String[] { "connect", "dev", "-u", "admin", "-p", "password" });

		if (cmd.hasOption("u")) {
			System.out.println(cmd.getOptionValue("u"));
		}
		if (cmd.hasOption("p")) {
			System.out.println(cmd.getOptionValue("u"));
		}
		System.out.println(cmd.getArgList());
	}

}
