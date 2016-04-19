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


import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.ff4j.FF4j;
import org.ff4j.cli.ansi.AnsiForegroundColor;
import org.ff4j.cli.ansi.AnsiTerminal;
import org.ff4j.cli.ansi.AnsiTextAttribute;
import org.ff4j.core.Feature;

/**
 * Render all component for the FF4J commands.
 *
 * @author @clunven
 */
public class FF4jCliDisplay {
	
	private static void white(String text) {
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.WHITE);
		System.out.print(text);
	}
	
	private static void yellow(String text) {
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
		System.out.print(text);
	}
	
	private static void cyan(String text) {
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.CYAN);
		System.out.print(text);
	}
	
	private static void green(String text) {
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.GREEN);
		System.out.print(text);
	}
	
	private static void lineHelp(String opt, String message) {
		System.out.println("");
		green("  " + StringUtils.rightPad(opt, 42));
		white(message);
	}
	
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
	
	public static void displayHelpNotConnected() {
		System.out.print("\nUsage (you are not connected) :");
		lineHelp("?, help", "Display this help");
		lineHelp("ls,list", "List available environments");
		lineHelp("connect <envName> (-u <user> -p <passwd>)", "Connect to target environments");
		lineHelp("exit,quit", "Exit the program");
		System.out.println("");
	}
	
	public static void displayHelpConnected() {
		System.out.print("\nUsage (you are connected):");
		lineHelp("?, help", "Display this help");
		lineHelp("uptime", "Display uptime");
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
	
	public static void displayEnvironments(Map < String, FF4j> envs) {
		AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
		System.out.println("+--------------------+---------------------------------------------------------+");
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
		
		for(Map.Entry<String, FF4j> entries : envs.entrySet()) {
			System.out.println("");
			green("  " + StringUtils.rightPad(entries.getKey(), 15));
			white("features: ");
			cyan(entries.getValue().getFeatures().keySet().toString());
		}
		System.out.println("");
	}
	
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
	
	public static void displayConf(FF4j ff4j) {
		System.out.println("Version :" + ff4j.getVersion());
		System.out.println("AutoCreate Enable :" + ff4j.isAutocreate());
		System.out.println("AuditEnable :" + ff4j.isEnableAudit());
		System.out.println("FeatureStore :" + ff4j.getFeatureStore().toString());
	}
	
	/**
	 * Command line uptime
	 */
	public static void displayUptime(long startTime) {
		long uptime = System.currentTimeMillis() - startTime;
        long daynumber = uptime / (1000 * 3600 * 24L);
        uptime = uptime - daynumber * 1000 * 3600 * 24L;
        long hourNumber = uptime / (1000 * 3600L);
        uptime = uptime - hourNumber * 1000 * 3600L;
        long minutenumber = uptime / (1000 * 60L);
        uptime = uptime - minutenumber * 1000 * 60L;
        long secondnumber = uptime / 1000L;
        
        StringBuilder sb = new StringBuilder();
        sb.append(daynumber + " day(s) ");
        sb.append(hourNumber + " hours(s) ");
        sb.append(minutenumber + " minute(s) ");
        sb.append(secondnumber + " second(s) ");
        AnsiTerminal.foreGroundColor(AnsiForegroundColor.YELLOW);
        System.out.print("Uptime : ");
        AnsiTerminal.foreGroundColor(AnsiForegroundColor.WHITE);
        System.out.println(sb.toString());
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
