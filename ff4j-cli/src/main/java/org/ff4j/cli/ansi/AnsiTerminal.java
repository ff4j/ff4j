package org.ff4j.cli.ansi;

import java.text.SimpleDateFormat;
import java.util.Date;

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


import org.ff4j.cli.util.OSSupported;
import org.ff4j.cli.util.OSUtil;
import org.fusesource.jansi.AnsiConsole;

/**
 * Work with terminal.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class AnsiTerminal implements AnsiConstants {
	
	private static final SimpleDateFormat SDF = new SimpleDateFormat("YYYY-MM-dd HH:mm:ss");

    /**
     * Change text color
     *
     * @param color
     *      blue color
     */
    public static void foreGroundColor(AnsiForegroundColor color) {
        setup(color, null, null);
    }
    
    /**
     * Change text color
     *
     * @param color
     *      blue color
     */
    public static void textAttribute(AnsiTextAttribute txt) {
        setup(null, null, txt);
    }

    /**
     * Change everything.
     *
     * @param color
     *      text color
     * @param backgroundColor
     * @param attribute
     */
    public static void setup(AnsiForegroundColor color, AnsiBackGroundColor backgroundColor, AnsiTextAttribute attribute) {
        // Create Ansi code
        StringBuilder sb = new StringBuilder(PREFIX);
        if (attribute != null) {
            sb.append(attribute.getCode());
        }
        sb.append(SEPARATOR);
        if (color != null) {
            sb.append(color.getCode());
        }
        sb.append(SEPARATOR);
        if (backgroundColor != null) {
            sb.append(backgroundColor.getCode());
        }
        sb.append(POSTFIX);

        // Apply to Terminal
        print(sb.toString());
    }
    
    public static void print(String text, AnsiForegroundColor color) {
        foreGroundColor(color);
        print(text);
    }
    
    /**
     * Log warning.
     *
     * @param text
     *      text to be displayed
     */
    public static void logWarn(String text) {
    	foreGroundColor(AnsiForegroundColor.WHITE);
    	System.out.print(SDF.format(new Date()));
        foreGroundColor(AnsiForegroundColor.YELLOW);
        System.out.print(" [WARN ] ");
        foreGroundColor(AnsiForegroundColor.WHITE);
        System.out.println(text);
    }
    
    /**
     * Log info.
     *
     * @param text
     *       text to be displayed
     */
    public static void logInfo(String text) {
    	foreGroundColor(AnsiForegroundColor.WHITE);
    	System.out.print(SDF.format(new Date()));
        foreGroundColor(AnsiForegroundColor.CYAN);
        System.out.print(" [INFO ] ");
        foreGroundColor(AnsiForegroundColor.WHITE);
        System.out.println(text);
    }
    
    /**
     * Log error.
     *
     * @param text
     *       text to be displayed
     */
    public static void logError(String text) {
    	foreGroundColor(AnsiForegroundColor.WHITE);
    	System.out.print(SDF.format(new Date()));
        foreGroundColor(AnsiForegroundColor.RED);
        System.out.print(" [ERROR] ");
        foreGroundColor(AnsiForegroundColor.WHITE);
        System.out.println(text);
    }
    
    /**
     * Print text ton console.
     *
     * @param text
     *      current text to be displauyed
     */
    public static void print(String text) {
        OSSupported os = OSUtil.getCurrentOS();
        if (os == null) {
            System.out.print(text);
            System.out.flush();
        } else {
            switch (os) {
                case WINDOWS:
                    AnsiConsole.out.print(text);
                    AnsiConsole.out.flush();
                break;
                case UNIX:
                case OS_X:
                case SOLARIS:
                    System.out.print(text);
                    System.out.flush();
                break;
            }
        }   
        
    }
    
    /**
     * Print text ton console.
     *
     * @param text
     *      current text to be displauyed
     */
    public static void println(String text) {
        print(text);
        System.out.println("\n");
    }
    
}
