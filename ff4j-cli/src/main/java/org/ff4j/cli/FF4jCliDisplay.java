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
import static org.ff4j.cli.ansi.AnsiTerminal.cyan;
import static org.ff4j.cli.ansi.AnsiTerminal.green;
import static org.ff4j.cli.ansi.AnsiTerminal.white;

import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.ff4j.FF4j;
import org.ff4j.cli.ansi.AnsiForegroundColor;
import org.ff4j.cli.ansi.AnsiTerminal;
import org.ff4j.cli.ansi.AnsiTextAttribute;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.property.Property;
import org.ff4j.property.store.PropertyStore;

/**
 * Render all component for the FF4J commands.
 *
 * @author @clunven
 */
public class FF4jCliDisplay {
	
	/**
	 * Default constructor.
	 */
	private FF4jCliDisplay() {
		throw new UnsupportedOperationException("Should no instanciate utility class");
	}
		
	/**
	 * Start Banner.
	 */
	public static void displayBanner() {
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.GREEN);
        System.out.println("  __  __ _  _   _ ");
        System.out.println(" / _|/ _| || | (_)");
        System.out.println("| |_| |_| || |_| |");
        System.out.println("|  _|  _|__   _| |");
        System.out.println("|_| |_|    |_|_/ |");
        System.out.print("             |__/   ");
        System.out.println("\n");
	}
	
	/**
	 * Display prompt like "ff4j@DEV>".
	 *
	 * @param currentEnv
	 * 		environment selected if exists
	 */
	public static void displayPrompt(String currentEnv) {
		System.out.println();
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.GREEN);
		System.out.print("ff4j");
		if (null != currentEnv) {
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.WHITE);
			System.out.print("@");
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
			System.out.print(currentEnv);
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.WHITE);
		}
		System.out.print(">");
		System.out.flush();
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.WHITE);
	}
	
	/**
	 * Template for helps.
	 *
	 * @param opt
	 * 		options
	 * @param message
	 * 		current messages
	 */
	private static void lineHelp(String opt, String message) {
		System.out.println("");
		green("  " + StringUtils.rightPad(opt, 42));
		white(message);
	}
	
	/**
	 * Help when no environnement is selected.
	 */
	public static void displayHelpNotConnected() {
		System.out.print("\nUsage (you are not connected) :");
		lineHelp("?, help", "Display this help");
		lineHelp("ls,list", "List available environments");
		lineHelp("connect <envName> (-u <user> -p <passwd>)", "Connect to target environments");
		lineHelp("exit,quit", "Exit the program");
		System.out.println("");
	}

	/**
	 * Help when an environnement is selected.
	 */
	public static void displayHelpConnected() {
		System.out.print("\nUsage (you are connected):");
		lineHelp("?, help", "Display this help");
		lineHelp("conf", "Display ff4j configuration for this env");
		lineHelp("list", "List all available elements");
		lineHelp("features", "List available features");
		lineHelp("properties", "List available properties");
		lineHelp("enable -f <featureName>", "Toggle ON feature");
		lineHelp("disable -f <featureName>", "Toggle OFF feature");
		lineHelp("grant -r <role> -f <featureName>", "Grant role on feature");
		lineHelp("revoke -r <role> -f <featureName>", "Revoke role on feature");
		lineHelp("enableGroup -g <groupName>", "Toggle ON group");
		lineHelp("disableGroup -g <groupName>", "Toggle ON group");
		lineHelp("update -p <property> -v <value>", "Toggle ON feature");
		lineHelp("quit", "Disconnect from current env");
		lineHelp("exit", "Exit the program");
		System.out.println("");
	}
	
	/**
	 * Display a table of available environments.
	 *
	 * @param envs
	 * 		environnements in config file
	 */
	public static void displayEnvironments(Map < String, FF4j> envs) {
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
		System.out.println("+--------------------+----------+------------+-------+----------+");
		System.out.print("|");
		AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.CYAN);
		System.out.print(" Environments       ");
		AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
		System.out.print("|");
		AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.CYAN);
		System.out.print(" Features ");
		AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
		System.out.print("|");
		AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.CYAN);
		System.out.print(" Properties ");
		AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
		System.out.print("|");
		AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.CYAN);
		System.out.print(" Audit ");
		AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
		System.out.print("|");
		AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.CYAN);
		System.out.print(" Security ");
		AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
		System.out.println("|");
		System.out.println("+--------------------+----------+------------+-------+----------+");
		
		for(Map.Entry<String, FF4j> entries : envs.entrySet()) {
			FeatureStore  fs = entries.getValue().getFeatureStore();
			PropertyStore ps = entries.getValue().getPropertiesStore();
			
			AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
			System.out.print("|  ");
			AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.GREEN);
			System.out.print(StringUtils.rightPad(entries.getKey(), 18));
			AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
			System.out.print("|  ");
			AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.WHITE);
			String featureStore = "---";
			if (fs != null) {
				featureStore = String.valueOf(fs.readAll().size());
			}
			System.out.print(StringUtils.rightPad(featureStore, 8));
			AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
			System.out.print("|  ");
			AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.WHITE);
			String propertyStore = "---";
			if (fs != null) {
				propertyStore = String.valueOf(ps.listPropertyNames().size());
			}
			System.out.print(StringUtils.rightPad(propertyStore, 10));
			AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
			System.out.print("|");
			if (entries.getValue().isEnableAudit()) {
				AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
				AnsiTerminal.foreGroundColor(AnsiForegroundColor.GREEN);
				System.out.print("  ON   ");
			} else {
				AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
				AnsiTerminal.foreGroundColor(AnsiForegroundColor.RED);
				System.out.print("  OFF  ");
			}
			AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
			System.out.print("|");
			if (entries.getValue().getAuthorizationsManager() != null) {
				AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
				AnsiTerminal.foreGroundColor(AnsiForegroundColor.GREEN);
				System.out.print("  ON      ");
			} else {
				AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
				AnsiTerminal.foreGroundColor(AnsiForegroundColor.RED);
				System.out.print("  OFF     ");
			}
			AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
			System.out.println("|");
		}
		System.out.println("+--------------------+----------+------------+-------+----------+");
		System.out.println("");
	}
	
	
	/**
	 * Configuration for a sample environnement.
	 *
	 * @param ff4j
	 * 		ff4j dedicated to the environnement.
	 */
	public static void displayConf(FF4j ff4j) {
		System.out.println("Version :" + ff4j.getVersion());
		System.out.println("AutoCreate Enable :" + ff4j.isAutocreate());
		System.out.println("AuditEnable :" + ff4j.isEnableAudit());
		System.out.println("FeatureStore :" + ff4j.getFeatureStore().toString());
	}
	
	/**
	 * Command line uptime
	 */
	public static void displayProperties(Map < String, Property<?> > properties) {
		if (properties == null || properties.isEmpty()) {
			System.out.println(" There are no properties in the store");
		}
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
		System.out.println("+--------------------+--------+---------------+--------------------------------+");
		System.out.print("|");
		AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.CYAN);
		System.out.print(" Property names      ");
		AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
		System.out.print("|");
		AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.CYAN);
		System.out.print(" Value  ");
		AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
		System.out.print("|");
		AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.CYAN);
		System.out.print(" Type  ");
		AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
		System.out.print("|");
		AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.CYAN);
		System.out.print(" FixedValues  ");
		AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
		System.out.println("|");
		System.out.println("+--------------------+--------+---------------+--------------------------------+");
		
	}
	
	/**
	 * Command line uptime
	 */
	public static void displayFeatures(Map < String, Feature > features) {
		if (features == null || features.isEmpty()) {
			System.out.println(" There are no features in the store");
		}
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
		System.out.println("+--------------------+--------+---------------+--------------------------------+");
		System.out.print("|");
		AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.CYAN);
		System.out.print(" Feature names      ");
		AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
		System.out.print("|");
		AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.CYAN);
		System.out.print(" State  ");
		AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
		System.out.print("|");
		AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.CYAN);
		System.out.print(" Group         ");
		AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
		System.out.print("|");
		AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.CYAN);
		System.out.print(" Roles                          ");
		AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
		System.out.println("|");
		System.out.println("+--------------------+--------+---------------+--------------------------------+");
		for (Feature feat : features.values()) {
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
			System.out.print("| ");
			AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.GREEN);
			System.out.print(StringUtils.rightPad(feat.getUid(), 19));
			
			AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
			System.out.print("| ");
			if (feat.isEnable()) {
				AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
				AnsiTerminal.foreGroundColor(AnsiForegroundColor.GREEN);
				System.out.print(StringUtils.rightPad("ON", 7));
			} else {
				AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
				AnsiTerminal.foreGroundColor(AnsiForegroundColor.RED);
				System.out.print(StringUtils.rightPad("OFF", 7));
			}
			
			AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
			System.out.print("| ");
			AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.WHITE);
			String groupName = "---";  
			if (!StringUtils.isEmpty(feat.getGroup())) {
				groupName = feat.getGroup();
			}
			System.out.print(StringUtils.rightPad(groupName, 14));
			
			AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
			System.out.print("| ");
			AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.WHITE);
			String roles = feat.getPermissions().toString();
			roles = roles.substring(1, roles.length()-1);
			if ("".equals(roles)) {
				roles = "---";
			}
			System.out.print(StringUtils.rightPad(roles, 31));
			
			AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
			System.out.println("| ");
		}
		AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
		System.out.println("+--------------------+--------+---------------+--------------------------------+");
	}

}
