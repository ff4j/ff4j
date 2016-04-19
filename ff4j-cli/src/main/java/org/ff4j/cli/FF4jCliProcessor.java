package org.ff4j.cli;

import static org.ff4j.cli.FF4jCliDisplay.displayEnvironments;
import static org.ff4j.cli.FF4jCliDisplay.displayHelpNotConnected;
import static org.ff4j.cli.FF4jCliOptions.connectOptions;

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

import static org.ff4j.cli.ansi.AnsiTerminal.logError;
import static org.ff4j.cli.ansi.AnsiTerminal.logInfo;
import static org.ff4j.cli.ansi.AnsiTerminal.logWarn;
import static org.ff4j.cli.ansi.AnsiTerminal.foreGroundColor;
import static org.ff4j.cli.ansi.AnsiTerminal.*;
import static org.ff4j.cli.FF4jCliDisplay.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.ParseException;
import org.apache.commons.lang.StringUtils;
import org.ff4j.FF4j;
import org.ff4j.cli.ansi.AnsiForegroundColor;
import org.ff4j.cli.ansi.AnsiTerminal;
import org.ff4j.cli.ansi.AnsiTextAttribute;
import org.springframework.context.support.ClassPathXmlApplicationContext;

/**
 * Command processor.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class FF4jCliProcessor {

	/** Commons-cli command parser. */
	private static final CommandLineParser CMD_PARSER = new DefaultParser();
	
	/** Environnements. */
	private Map<String, FF4j> envs = new LinkedHashMap<String, FF4j>();

	/** Users. */
	private Map<String, String> users = new LinkedHashMap<String, String>();

	/** Current environment. */
	private String currentEnv = null;

	/** Current environment. */
	private FF4j currentFF4J = null;
	
	/**
	 * Default Constructor.
	 */
	public FF4jCliProcessor() {
	}
	
	/**
	 * Sample Processor.
	 * @param springConfFile
	 */
	public FF4jCliProcessor(String springConfFile) {
		parseSpringContext(springConfFile);
	}

	/**
	 * Command are not the same if you have selected an environnement or not.
	 *
	 * @param commandLine
	 * 		current command lien
	 */
	public void evaluate(String commandLine) {
		if (currentEnv == null) {
			dispatchCommandNotConnected(commandLine);
		} else {
			dispatchCommandConnected(commandLine);
		}
	}
	
	/**
	 * Input provided by user.
	 *
	 * @param commandLine
	 * 		current command line
	 */
	private void dispatchCommandNotConnected(String commandLine) {
		if (commandLine.startsWith("help") || commandLine.startsWith("?")) {
			displayHelpNotConnected();
		} else if (commandLine.equals("list") || commandLine.equals("ls")) {
			displayEnvironments(envs);
		} else if (commandLine.startsWith("exit") || commandLine.startsWith("quit")) {
			exit();
		} else if (commandLine.startsWith("connect")) {
			processCommandConnect(commandLine);
		} else {
			logWarn("Invalid command, not recognized");
			displayHelpNotConnected();
		}
	}
	
	/**
	 * Element for connected commands.
	 *
	 * @param commandLine
	 * 			command 
	 */
	private void dispatchCommandConnected(String commandLine) {
		String[] cmdParts = commandLine.split(" ");
		String cmd = cmdParts[0].toLowerCase().trim();
		if (cmd.equals("quit")) {
			currentEnv  = null;
			currentFF4J = null;
			
		} else if (cmd.equals("exit") ) {
			exit();
			
		} else if (cmd.equals("help") || cmd.equals("?")) {
			displayHelpConnected();
			
		} else if (cmd.equals("features")) {
			displayFeatures(currentFF4J.getFeatureStore().readAll());
			
		} else if (cmd.equals("properties")) {
			displayProperties(currentFF4J.getPropertiesStore().readAllProperties());
			red("Not yet available");
			
		} else if (cmd.equals("list")) {
			red("Not yet available");
			
		} else if (cmd.equals("enable")) {
			red("Not yet available");
			
		} else if (cmd.equals("disable")) {
			red("Not yet available");
			
		} else if (cmd.equals("grant")) {
			red("Not yet available");
			
		}  else if (cmd.equals("revoke")) {
			red("Not yet available");
			
		} else if (cmd.equals("enableGroup")) {
			red("Not yet available");
			
		} else if (cmd.equals("disableGroup")) {
			red("Not yet available");
			
		} else if (cmd.equals("update")) {
			red("Not yet available");
			
		} else if (cmd.equals("enableAudit")) {
			red("Not yet available");
			
		} else {
			logWarn("Invalid command, not recognized");
			FF4jCliDisplay.displayHelpConnected();
		}
	}
	
	/**
	 * Command to connect.
	 *
	 * @param commandLine
	 * 			execute command line
	 */
	private void processCommandConnect(String commandLine) {
		try {
			CommandLine cmd = CMD_PARSER.parse(connectOptions(), commandLine.split(" "));
			if (cmd.getArgList().size() != 2) {
				logWarn("Invalid command, expecting connect <envName> [-u user] [-p password]");
			} else if (users.isEmpty()) {
				connectEnv(cmd.getArgList().get(1));
			} else if (!cmd.hasOption("u") || !cmd.hasOption("p")) {
				logWarn("Connection is not setup as opened, expecting credentials");
				logWarn("Invalid syntax expected connect <envName> -u <user> -p <password>");
			} else {
				String user     = cmd.getOptionValue('u');
				String password = cmd.getOptionValue('p');
				if (!users.containsKey(user) || !users.get(user).equals(password)) {
					logWarn("Invalid credentials, check users");
				} else {
					connectEnv(cmd.getArgList().get(1));
				}
			}
		} catch (ParseException e) {
			error(e, "Error during connect command");
		}
	}
	
	/**
	 * Selecting environnement.
	 *
	 * @param envName
	 * 		target environment name
	 */
	private void connectEnv(String envName) {
		currentEnv  = envName;
		currentFF4J = envs.get(currentEnv);
		if (currentFF4J == null) {
			logWarn("Invalid environment name, please check");
			displayEnvironments(envs);
			currentEnv  = null;
		} else {
			logInfo("Environment [" + currentEnv + "] is now selected");
		}
	}
	
	/**
	 * Parse Spring context.
	 */
	@SuppressWarnings("unchecked")
	public void parseSpringContext(String fileName) {
		try {
			logInfo("Loading configurations from classpath file [" + fileName + "]");
			ClassPathXmlApplicationContext ctx = new ClassPathXmlApplicationContext(fileName);
			this.envs = ctx.getBeansOfType(FF4j.class);
			if (ctx.containsBean("AUTHORIZED_USERS")) {
				this.users = (Map<String, String>) ctx.getBean("AUTHORIZED_USERS");
			}
			ctx.close();
		} catch (RuntimeException fne) {
			error(fne, "Cannot parse Spring context");
		}
	}
	
	/**
	 * Exit
	 */
	private void exit() {
		logInfo("Exiting FF4j... Good Bye");
		foreGroundColor(AnsiForegroundColor.WHITE);
		textAttribute(AnsiTextAttribute.CLEAR);
		System.exit(0);
	}
	
	/**
	 * Error.
	 * 
	 * @param t
	 * 		current erorr
	 * @param message
	 */
	private void error(Throwable t, String message) {
		if (t != null) {
			logError(t.getClass().getName() + " : " + t.getMessage());
		} else {
			logError(message);
		}
		System.exit(-1);
	}

	/**
	 * @return the envs
	 */
	public Map<String, FF4j> getEnvs() {
		return envs;
	}

	/**
	 * @param envs the envs to set
	 */
	public void setEnvs(Map<String, FF4j> envs) {
		this.envs = envs;
	}

	/**
	 * @return the users
	 */
	public Map<String, String> getUsers() {
		return users;
	}
	
	/**
	 * @return the currentEnv
	 */
	public String getCurrentEnv() {
		return currentEnv;
	}
	
	/**
	 * @return the currentFF4J
	 */
	public FF4j getCurrentFF4J() {
		return currentFF4J;
	}	
}
