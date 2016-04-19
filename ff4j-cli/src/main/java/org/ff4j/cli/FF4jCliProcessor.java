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


import static org.ff4j.cli.ansi.AnsiTerminal.logError;
import static org.ff4j.cli.ansi.AnsiTerminal.logInfo;
import static org.ff4j.cli.ansi.AnsiTerminal.logWarn;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.ff4j.FF4j;
import org.ff4j.cli.ansi.AnsiForegroundColor;
import org.ff4j.cli.ansi.AnsiTerminal;
import org.ff4j.cli.ansi.AnsiTextAttribute;
import org.ff4j.core.Feature;
import org.springframework.context.support.ClassPathXmlApplicationContext;

/**
 * Command processor.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class FF4jCliProcessor {

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
	 * execute command.
	 *
	 * @param commandLine
	 * 		current command lien
	 */
	public void processCommandLine(String commandLine) {
		if (currentEnv == null) {
			commandNotConnected(commandLine);
		} else {
			commandConnected(commandLine);
		}
	}
	
	/**
	 * Input provided by user.
	 *
	 * @param commandLine
	 * 		current command line
	 */
	private void commandNotConnected(String commandLine) {
		List<String> params = new ArrayList<String>(Arrays.asList(commandLine.split(" ")));
		
		if (commandLine.startsWith("help") || commandLine.startsWith("?")) {
			FF4jCliDisplay.displayHelpNotConnected();
			
		} else if (commandLine.equals("list") || commandLine.equals("ls")) {
			FF4jCliDisplay.displayEnvironments(envs);
			
		} else if (commandLine.startsWith("exit") || commandLine.startsWith("quit")) {
			exit();
			
		} else if (commandLine.equals("enableAudit")) {
			
			
		} else if (commandLine.startsWith("connect")) {
			
			logInfo("Selecting [" + params.get(1) + "]");
			if (users.isEmpty()) {
				connectEnv(params.get(1));
			} else {
				// -u and -p mandatory
				int idxIser = params.indexOf("-u");
				int idxPassword = params.indexOf("-p");
				if (idxIser == -1 || idxPassword == -1 || 
						params.size() < (idxIser+1) || 
						params.size() < (idxPassword+1)) {
					logWarn("Connection is not setup as opened, expecting credentials");
					logWarn("Invalid syntax expected connect <envName> -u <user> -p <password>");
				} else {
					// user and password
					String user = params.get(idxIser+1);
					String password = params.get(idxPassword+1);
					if (!users.containsKey(user) || 
							!users.get(user).equals(password)) {
						logWarn("Invalid credentials, check users");
					} else {
						connectEnv(params.get(1));
					}
				}
			}
		} else {
			logWarn("Invalid command, not recognized");
			FF4jCliDisplay.displayHelpNotConnected();
		}
	}
	
	private void connectEnv(String envName) {
		currentEnv = envName;
		currentFF4J = envs.get(currentEnv);
		if (currentFF4J == null) {
			logWarn("Invalid environment name, please check");
			FF4jCliDisplay.displayEnvironments(envs);
		} else {
			logInfo("Environment [" + currentEnv + "] is now selected");
		}
	}
	
	/**
	 * 
	 * @param commandLine
	 * 			command 
	 */
	private void commandConnected(String commandLine) {
		List<String> params = new ArrayList<String>(Arrays.asList(commandLine.split(" ")));
		
		String cmd = params.get(0).toLowerCase().trim();
		if (cmd.equals("quit")) {
			currentEnv  = null;
			currentFF4J = null;
		} else if (cmd.equals("exit") ) {
			exit();
		} else if (cmd.equals("help") || cmd.equals("?")) {
			FF4jCliDisplay.displayHelpConnected();
		} else if (cmd.equals("uptime")) {
			FF4jCliDisplay.displayUptime(currentFF4J.getStartTime());
		} else if (cmd.equals("features")) {
			FF4jCliDisplay.displayFeatures(currentFF4J.getFeatureStore().readAll());
		} else if (cmd.equals("properties")) {
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.RED);
			System.out.println("Not yet available");
		} else if (cmd.equals("list")) {
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.RED);
			System.out.println("Not yet available");
		} else if (cmd.equals("enable")) {
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.RED);
			System.out.println("Not yet available");
		} else if (cmd.equals("disable")) {
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.RED);
			System.out.println("Not yet available");
		} else if (cmd.equals("grant")) {
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.RED);
			System.out.println("Not yet available");
		}  else if (cmd.equals("revoke")) {
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.RED);
			System.out.println("Not yet available");
		} else if (cmd.equals("enableGroup")) {
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.RED);
			System.out.println("Not yet available");
		} else if (cmd.equals("disableGroup")) {
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.RED);
			System.out.println("Not yet available");
		} else if (cmd.equals("update")) {
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.RED);
			System.out.println("Not yet available");
		} else {
			logWarn("Invalid command, not recognized");
			FF4jCliDisplay.displayHelpConnected();
		}
	}

	/**
	 * Parse Spring context.
	 */
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
			error(fne, "Cannot read configuration file, check conf folder.");
		}
	}
	
	private void exit() {
		logInfo("Exiting FF4j... Good Bye");
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.WHITE);
		AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
		System.exit(0);
	}
	
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
