package org.ff4j.cli.ansi;

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


import java.text.SimpleDateFormat;
import java.util.Date;

import org.ff4j.cli.util.OSSupported;
import org.ff4j.cli.util.OSUtil;
import org.fusesource.jansi.AnsiConsole;

/**
 * Work with terminal.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class AnsiTerminal implements AnsiConstants {
	
	/** Default log format. */
	private static final SimpleDateFormat SDF = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

	/**
	 * Hide default  constructor.
	 */
	private AnsiTerminal() {
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

        print(sb.toString());
    }
    
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
     * Print text ton console.
     *
     * @param text
     *      current text to be displauyed
     */
    public static void print(String text) {
        print(text, OSUtil.getCurrentOS());
    }
    
    public static void print(String text, OSSupported os) {
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
     * Output.
     *
     * @param text
     * @param color
     */
    public static void print(String text, AnsiForegroundColor color) {
        foreGroundColor(color);
        print(text);
    }
    
    /**
	 * Change text color to white.
	 */
	public static void white(String text) {
		print(text, AnsiForegroundColor.WHITE);
	}
	
	/**
	 * Change text color to yellow.
	 */
	public static void yellow(String text) {
		print(text, AnsiForegroundColor.YELLOW);
	}
	
	/**
	 * Change text color to red.
	 */
	public static void red(String text) {
		print(text, AnsiForegroundColor.RED);
	}
	
	/**
	 * Change text color to cyan.
	 */
	public static void cyan(String text) {
		print(text, AnsiForegroundColor.CYAN);
	}
	
	/**
	 * Change text color to green.
	 */
	public static void green(String text) {
		print(text, AnsiForegroundColor.GREEN);
	}
	
	/**
     * Log warning.
     *
     * @param text
     *      text to be displayed
     */
    public static void logWarn(String text) {
    	white(SDF.format(new Date()));
    	yellow(" [WARN ] ");
    	white(text);
    	System.out.println("");
    }
    
    /**
     * Log info.
     *
     * @param text
     *       text to be displayed
     */
    public static void logInfo(String text) {
    	white(SDF.format(new Date()));
    	cyan(" [INFO ] ");
    	white(text);
    	System.out.println("");
    }
    
    /**
     * Log error.
     *
     * @param text
     *       text to be displayed
     */
    public static void logError(String text) {
    	white(SDF.format(new Date()));
    	red(" [ERROR] ");
    	white(text);
    	System.out.println("");
    }
    
}
