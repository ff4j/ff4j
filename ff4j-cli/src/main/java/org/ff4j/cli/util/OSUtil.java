package org.ff4j.cli.util;

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


/**
 * Get current OS
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class OSUtil {
    
    /** Current OS Value. */
    private static String OS = System.getProperty("os.name").toLowerCase();
    
    private static OSSupported currentOS = null;
    
    /**
     * Hide default constructor.
     */
    private OSUtil() {
    }
    
    /**
     * Retrieve OS.
     *
     * @return
     */
    public static OSSupported getCurrentOS() {
        if (currentOS ==null) {
            if (OS.contains("win")) {
                currentOS = OSSupported.WINDOWS;
            } else if (OS.contains("mac")) {
                currentOS = OSSupported.OS_X;
            } else if (OS.contains("nix") || OS.contains("nux")|| OS.contains("aix")) {
                currentOS = OSSupported.UNIX;
            } else if (OS.contains("sunos")) {
                currentOS = OSSupported.SOLARIS;
            }
        }
        return currentOS;
    }

}
