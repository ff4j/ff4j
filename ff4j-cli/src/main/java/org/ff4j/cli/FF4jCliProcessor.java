package org.ff4j.cli;

import static org.ff4j.cli.FF4jCliDisplay.displayConf;
import static org.ff4j.cli.FF4jCliDisplay.displayEnvironments;
import static org.ff4j.cli.FF4jCliDisplay.displayFeatures;
import static org.ff4j.cli.FF4jCliDisplay.displayHelpConnected;
import static org.ff4j.cli.FF4jCliDisplay.displayHelpNotConnected;
import static org.ff4j.cli.FF4jCliDisplay.displayProperties;
import static org.ff4j.cli.FF4jCliOptions.addGroupOptions;
import static org.ff4j.cli.FF4jCliOptions.connectOptions;
import static org.ff4j.cli.FF4jCliOptions.enableFeatureOptions;
import static org.ff4j.cli.FF4jCliOptions.enableGroupOptions;
import static org.ff4j.cli.FF4jCliOptions.grantOptions;
import static org.ff4j.cli.FF4jCliOptions.propertyOptions;
import static org.ff4j.cli.ansi.AnsiTerminal.foreGroundColor;

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
import static org.ff4j.cli.ansi.AnsiTerminal.textAttribute;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
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

	/** String constants */
	private static final String FEATURE = "Feature ";
	private static final String ERROR_DURING_CONNECT_COMMAND = "Error during connect command";

	/** Environnements. */
	private Map<String, FF4j> envs = new LinkedHashMap<String, FF4j>();

	/** Users. */
	private Map<String, String> users = new LinkedHashMap<String, String>();

	/** Current environment. */
	private String currentEnv = null;

	/** Current environment. */
	private FF4j currentFF4J = null;
	
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
		} else if (!commandLine.isEmpty()) {
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
		String cmd = cmdParts[0].trim();
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
			
		} else if (cmd.equals("conf")) {
			displayConf(currentFF4J);
			
		}else if (cmd.equals("list") || cmd.equals("ls")) {
			AnsiTerminal.white("\nFeatures:\n");
			displayFeatures(currentFF4J.getFeatureStore().readAll());
			
			AnsiTerminal.white("\nProperties:\n");
			displayProperties(currentFF4J.getPropertiesStore().readAllProperties());
			
		} else if (cmd.equals("enable")) {
			processCommandEnable(commandLine, true);
			System.out.println("");
			displayFeatures(currentFF4J.getFeatureStore().readAll());
			
		} else if (cmd.equals("disable")) {
			processCommandEnable(commandLine, false);
			displayFeatures(currentFF4J.getFeatureStore().readAll());
			
		} else if (cmd.equals("enableGroup")) {
			processCommandEnableGroup(commandLine, true);
			displayFeatures(currentFF4J.getFeatureStore().readAll());
			
		} else if (cmd.equals("disableGroup")) {
			processCommandEnableGroup(commandLine, false);
			displayFeatures(currentFF4J.getFeatureStore().readAll());
			
		} else if (cmd.equals("addToGroup")) {
			processCommandAddGroup(commandLine);
			displayFeatures(currentFF4J.getFeatureStore().readAll());
			
		} else if (cmd.equals("removeFromGroup")) {
			processCommandAddGroup(commandLine);
			displayFeatures(currentFF4J.getFeatureStore().readAll());
			
		} else if (cmd.equals("grant")) {
			processCommandGrant(commandLine);
			displayFeatures(currentFF4J.getFeatureStore().readAll());
			
		}  else if (cmd.equals("revoke")) {
			processCommandGrant(commandLine);
			displayFeatures(currentFF4J.getFeatureStore().readAll());
			
		} else if (cmd.equals("enableAudit")) {
			processCommandEnableEnableAudit(commandLine, true);
			Map <String, FF4j> mappi = new HashMap<String, FF4j>();
			mappi.put(currentEnv, currentFF4J);
			System.out.println("");
			displayEnvironments(mappi);
			
		}  else if (cmd.equals("disableAudit")) {
			processCommandEnableEnableAudit(commandLine, false);
			Map <String, FF4j> mappi = new HashMap<String, FF4j>();
			mappi.put(currentEnv, currentFF4J);
			System.out.println("");
			displayEnvironments(mappi);
			
		} else if (cmd.equals("update")) {
			processCommandUpdateProperty(commandLine);
			System.out.println("");
			displayProperties(currentFF4J.getPropertiesStore().readAllProperties());
			
		} else {
			logWarn("Invalid command, not recognized");
			FF4jCliDisplay.displayHelpConnected();
		}
	}
	
	private void processCommandUpdateProperty(String commandLine) {
		try {
			CommandLine cmd = CMD_PARSER.parse(propertyOptions(), commandLine.split(" "));
			if (cmd.getArgList().size() != 1 || !cmd.hasOption("p") || !cmd.hasOption("v")) {
				logError("Invalid command, expecting update -p <property> -v <value>");
			} else {
				String property = cmd.getOptionValue('p');
				String value    = cmd.getOptionValue('v');
				if (!currentFF4J.getPropertiesStore().existProperty(property)) {
					logWarn("Property " + property + " does not exist, nothing to update");
				} else {
					currentFF4J.getPropertiesStore().updateProperty(property, value);
					logInfo("Property " + property + " has been updated with " + value);
				}
			}
		} catch (ParseException e) {
			error(e, "parsing error during update property command");
		} catch (Exception e) {
			error(e, "Cannot update property");
		}
	}
	
	private void processCommandAddGroup(String commandLine) {
		try {
			CommandLine cmd = CMD_PARSER.parse(addGroupOptions(), commandLine.split(" "));
			if (cmd.getArgList().size() != 1 || !cmd.hasOption("f") || !cmd.hasOption("g")) {
				logError("Invalid command, expecting addToGroup[removeFromGroup] -f <featureName> -g <grouName>");
			} else {
				String feature = cmd.getOptionValue('f');
				String group   = cmd.getOptionValue('g');
				if (!currentFF4J.getFeatureStore().exist(feature)) {
					logWarn("Feature does not exist, nothing updated");
				} else {
					if (cmd.getArgList().get(0).equals("addToGroup")) {
						currentFF4J.getFeatureStore().addToGroup(feature, group);
						logInfo(FEATURE + feature + " has been added to group " + group);
					} else if (cmd.getArgList().get(0).equals("removeFromGroup")) {
						String currentGroup = currentFF4J.getFeatureStore().read(feature).getGroup();
						if (group.equals(currentGroup)) {
							currentFF4J.getFeatureStore().removeFromGroup(feature, group);
							logInfo(FEATURE + feature + " has been removed from group: " + group);
						} else if (currentGroup == null || currentGroup.isEmpty()){
							logWarn("The groupName is invalid expected:" + currentGroup + " but was [" + group + "]");
						} else {
							logWarn("Cannot remove group: there are no group on this feature");
						}
					}
				}
			}
		} catch (ParseException e) {
			error(e, "Error during addToGroup/removeFromGroup command");
		}
	}
	
	private void processCommandGrant(String commandLine) {
		try {
			CommandLine cmd = CMD_PARSER.parse(grantOptions(), commandLine.split(" "));
			if (cmd.getArgList().size() != 1 || !cmd.hasOption("f") || !cmd.hasOption("r")) {
				logError("Invalid command, expecting grant[revoke] -r <role> -f <featureName>");
			} else {
				String feature = cmd.getOptionValue('f');
				String role    = cmd.getOptionValue('r');
				if (!currentFF4J.getFeatureStore().exist(feature)) {
					logWarn("Feature does not exist, nothing updated");
				} else {
					if (cmd.getArgList().get(0).equals("grant")) {
						currentFF4J.getFeatureStore().grantRoleOnFeature(feature, role);
						logInfo("Role " + role + " has been added to feature " + feature);
					} else if (cmd.getArgList().get(0).equals("revoke")) {
						Set< String > permissions = currentFF4J.getFeatureStore().read(feature).getPermissions();
						if (permissions == null) {
							logWarn("The role is invalidn there is no role on the feature " + feature);
						} else if (permissions.contains(role)) {
							currentFF4J.getFeatureStore().removeRoleFromFeature(feature, role);
							logInfo(FEATURE + feature + " has not more role " + role);
						} else {
							logWarn("The role is invalid expected one of " + permissions.toString());
						}
					}
				}
			}
		} catch (ParseException e) {
			error(e, "Error during addToGroup/removeFromGroup command");
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
				logError("Invalid command, expecting connect <envName> [-u user] [-p password]");
			} else if (users.isEmpty()) {
				connectEnv(cmd.getArgList().get(1));
			} else if (!cmd.hasOption("u") || !cmd.hasOption("p")) {
				logWarn("Connection is not setup as opened, expecting credentials");
				logError("Invalid syntax expected connect <envName> -u <user> -p <password>");
			} else {
				String user     = cmd.getOptionValue('u');
				String password = cmd.getOptionValue('p');
				if (!users.containsKey(user) || !users.get(user).equals(password)) {
					logError("Invalid credentials, check users");
				} else {
					connectEnv(cmd.getArgList().get(1));
				}
			}
		} catch (ParseException e) {
			error(e, ERROR_DURING_CONNECT_COMMAND);
		}
	}
	
	/**
	 * Process commandline when an environment is already selected.
	 *
	 * @param commandLine
	 * 		current command line
	 * @param enable
	 * 		flag to disable or enable features
	 */
	private void processCommandEnable(String commandLine, boolean enable) {
		try {
			CommandLine cmd = CMD_PARSER.parse(enableFeatureOptions(), commandLine.split(" "));
			if (cmd.getArgList().size() != 1 || !cmd.hasOption("f") ) {
				logWarn("Invalid command, expecting enable/disable -f <featureName>");
			} else {
				String featureName = cmd.getOptionValue('f');
				if (!currentFF4J.getFeatureStore().exist(featureName)) {
					logWarn("Feature [" + featureName + "] not found");
					
				} else if (enable){
					currentFF4J.getFeatureStore().enable(featureName);
					logInfo(FEATURE + featureName + " is now enabled") ;
					
				} else {
					currentFF4J.getFeatureStore().disable(featureName);
					logInfo(FEATURE + featureName + " is now disabled") ;
				}
			}
		} catch (ParseException moe) {
			error(moe, ERROR_DURING_CONNECT_COMMAND);
		}
	}
	
	private void processCommandEnableEnableAudit(String commandLine, boolean enable) {
		try {
			CMD_PARSER.parse(new Options(), commandLine.split(" "));
			currentFF4J.setEnableAudit(enable);
			if (enable) {
				logInfo("Audit enabled for environment " + currentEnv);
			} else {
				logInfo("Audit disabled for environment " + currentEnv);
			}
		} catch (ParseException e) {
			error(e, "Error during enableAudit command");
		}
	}
	
	private void processCommandEnableGroup(String commandLine, boolean enable) {
		try {
			CommandLine cmd = CMD_PARSER.parse(enableGroupOptions(), commandLine.split(" "));
			if (cmd.getArgList().size() != 1 || !cmd.hasOption("g") ) {
				logWarn("Invalid command, expecting enableGroup/disableGroup -f <groupName>");
			} else {
				String groupName = cmd.getOptionValue('g');
				if (!currentFF4J.getFeatureStore().existGroup(groupName)) {
					logWarn("Group [" + groupName + "] not found");
				} else if (enable){
					currentFF4J.getFeatureStore().enableGroup(groupName);
					logInfo("Group " + groupName + " is now enabled") ;
				} else {
					currentFF4J.getFeatureStore().disableGroup(groupName);
					logInfo("Group " + groupName + " is now disabled") ;
				}
			}
		} catch (ParseException e) {
			error(e, ERROR_DURING_CONNECT_COMMAND);
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
		logError(t.getClass().getName() + " : " + t.getMessage() + "[" + message + "]");
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
