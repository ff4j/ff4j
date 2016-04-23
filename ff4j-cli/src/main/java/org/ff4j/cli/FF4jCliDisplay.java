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
import static org.ff4j.cli.ansi.AnsiTerminal.white;
import static org.ff4j.cli.ansi.AnsiTerminal.yellow;
import static org.ff4j.cli.ansi.AnsiTerminal.green;
import static org.ff4j.cli.ansi.AnsiTerminal.red;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.ff4j.FF4j;
import org.ff4j.cli.ansi.AnsiForegroundColor;
import org.ff4j.cli.ansi.AnsiTerminal;
import org.ff4j.cli.ansi.AnsiTextAttribute;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyBigDecimal;
import org.ff4j.property.PropertyBigInteger;
import org.ff4j.property.PropertyBoolean;
import org.ff4j.property.PropertyByte;
import org.ff4j.property.PropertyDouble;
import org.ff4j.property.PropertyFloat;
import org.ff4j.property.PropertyInt;
import org.ff4j.property.PropertyLogLevel;
import org.ff4j.property.PropertyLong;
import org.ff4j.property.PropertyShort;
import org.ff4j.property.PropertyString;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.utils.Util;

/**
 * Render all component for the FF4J commands.
 *
 * @author @clunven
 */
public class FF4jCliDisplay {
	
	/** Mapping from simple 'String' <=> 'org.ff4j.property.PropertyString'. */
    private static Map < String , String > uxTypes = new HashMap< String , String>();
    
    /**
     * Initialized Primitive to work with Properties.
     */
    static {
        uxTypes.put(Byte.class.getSimpleName(), PropertyByte.class.getName());
        uxTypes.put(Short.class.getSimpleName(), PropertyShort.class.getName());
        uxTypes.put(Integer.class.getSimpleName(), PropertyInt.class.getName());
        uxTypes.put(Long.class.getSimpleName(), PropertyLong.class.getName());
        uxTypes.put(Double.class.getSimpleName(), PropertyDouble.class.getName());
        uxTypes.put(Boolean.class.getSimpleName(), PropertyBoolean.class.getName());
        uxTypes.put(Float.class.getSimpleName(), PropertyFloat.class.getName());
        uxTypes.put(BigInteger.class.getSimpleName(), PropertyBigInteger.class.getName());
        uxTypes.put(BigDecimal.class.getSimpleName(), PropertyBigDecimal.class.getName());
        uxTypes.put("LogLevel", PropertyLogLevel.class.getName());
        uxTypes.put(String.class.getSimpleName(), PropertyString.class.getName());
    }
    
	/**
	 * Default constructor.
	 */
	private FF4jCliDisplay() {
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
		cyan("  " + StringUtils.rightPad(opt, 43));
		white(message);
	}
	
	/**
	 * Help when no environnement is selected.
	 */
	public static void displayHelpNotConnected() {
		System.out.print("\nUsage (you are not connected) :");
		lineHelp("?, help", "Display this help");
		lineHelp("ls,list", "List available environments");
		lineHelp("connect <envName> [-u <user>] [-p <passwd>]", "Connect to target environments");
		lineHelp("exit,quit", "Exit the program\n");
	}

	/**
	 * Help when an environnement is selected.
	 */
	public static void displayHelpConnected() {
		System.out.print("\nUsage (you are connected):");
		lineHelp("?, help", "Display this help");
		lineHelp("conf", "Display ff4j configuration for this env");
		lineHelp("ls,list", "List all available elements");
		lineHelp("features", "List available features");
		lineHelp("properties", "List available properties");
		lineHelp("enableAudit", "Enable audit capability for current");
		lineHelp("disableAudit", "Disable audit capability for current");
		lineHelp("enable -f <feature>", "Toggle ON feature");
		lineHelp("disable -f <feature>", "Toggle OFF feature");
		lineHelp("grant -r <role> -f <feature>", "Grant role on feature");
		lineHelp("revoke -r <role> -f <feature>", "Revoke role on feature");
		lineHelp("enableGroup -g <group>", "Toggle ON group");
		lineHelp("disableGroup -g <group>", "Toggle OFF group");
		lineHelp("addToGroup -f <feature> -g <group>", "Add feature to target group");
		lineHelp("removeFromGroup -f <feature> -g <group>", "Remove feature to target group");
		lineHelp("update -p <property> -v <value>", "Update Property value");
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
		yellow("+--------------------+----------+------------+-------+----------+\n");
		System.out.print("|");
		cyan(" Environments       ");
		yellow("|");
		cyan(" Features ");
		yellow("|");
		cyan(" Properties ");
		yellow("|");
		cyan(" Audit ");
		yellow("|");
		cyan(" Security ");
		yellow("|\n");
		System.out.println("+--------------------+----------+------------+-------+----------+");
		for(Map.Entry<String, FF4j> entries : envs.entrySet()) {
			FeatureStore  fs = entries.getValue().getFeatureStore();
			PropertyStore ps = entries.getValue().getPropertiesStore();
			yellow("|  ");
			green(StringUtils.rightPad(entries.getKey(), 18));
			yellow("|  ");
			String featureStore = "---";
			if (fs != null) {
				featureStore = String.valueOf(fs.readAll().size());
			}
			white(StringUtils.rightPad(featureStore, 8));
			yellow("|  ");
			String propertyStore = "---";
			if (fs != null) {
				propertyStore = String.valueOf(ps.listPropertyNames().size());
			}
			white(StringUtils.rightPad(propertyStore, 10));
			yellow("|");
			AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
			if (entries.getValue().isEnableAudit()) {
				green("  ON   ");
			} else {
				red("  OFF  ");
			}
			AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
			yellow("|");
			AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
			if (entries.getValue().getAuthorizationsManager() != null) {
				green("  ON      ");
			} else {
				red("  OFF     ");
			}
			AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
			yellow("|\n");
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
		lineHelp("Version", ff4j.getVersion());
		lineHelp("Autocreate", String.valueOf(ff4j.isAutocreate()));
		lineHelp("Audit", String.valueOf(ff4j.isEnableAudit()));
		
		String authManager = "---";
		if (ff4j.getAuthorizationsManager() != null) {
			authManager = ff4j.getAuthorizationsManager().getClass().getName();
		}
		lineHelp("Authorization Manager", authManager);
		
		String featStore = "---";
		if (ff4j.getFeatureStore() != null) {
			featStore = ff4j.getFeatureStore().getClass().getName();
		}
		lineHelp("Feature Store", featStore);
		
		String propStore = "---";
		if (ff4j.getPropertiesStore() != null) {
			propStore = ff4j.getPropertiesStore().getClass().getName();
		}
		lineHelp("Property Store", propStore);
		
		String evtStore = "---";
		if (ff4j.getEventRepository() != null) {
			evtStore = ff4j.getEventRepository().getClass().getName();
		}
		lineHelp("Event Store", evtStore);
	}
	
	/**
	 * Command line uptime
	 */
	public static void displayProperties(Map < String, Property<?> > properties) {
		if (properties == null || properties.isEmpty()) {
			System.out.println(" There are no properties in the store");
		}
		yellow("+--------------------+--------------------+--------------------+--------------------------------+");
		System.out.print("\n|");
		cyan(" Property names     ");
		yellow("|");
		cyan(" Value              ");
		yellow("|");
		cyan(" Type               ");
		yellow("|");
		cyan(" FixedValues                    ");
		yellow("|");
		System.out.println("\n+--------------------+--------------------+--------------------+--------------------------------+");
		for (Property<?> prop : properties.values()) {
			yellow("| ");
			AnsiTerminal.textAttribute(AnsiTextAttribute.BOLD);
			AnsiTerminal.foreGroundColor(AnsiForegroundColor.GREEN);
			System.out.print(StringUtils.rightPad(prop.getName(), 19));
			AnsiTerminal.textAttribute(AnsiTextAttribute.CLEAR);
			yellow("| ");
			white(StringUtils.rightPad(prop.asString(), 19));
			yellow("| ");
			
			String pType = prop.getType();
			if (uxTypes.containsValue(prop.getType())) {
				pType = Util.getFirstKeyByValue(uxTypes, prop.getType());
	        }
			white(StringUtils.rightPad(pType, 19));
			yellow("| ");
			String fixedValues = "---";
			if (prop.getFixedValues() != null && !prop.getFixedValues().isEmpty()) {
				fixedValues = prop.getFixedValues().toString();
				if (fixedValues.length() > 31) {
					fixedValues = fixedValues.substring(0, 28) + "...";
				}
			}
			white(StringUtils.rightPad(fixedValues, 31));
			yellow("|\n");
		}
		System.out.println("+--------------------+--------------------+--------------------+--------------------------------+");
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
