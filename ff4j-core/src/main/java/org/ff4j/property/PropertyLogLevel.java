package org.ff4j.property;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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

import java.util.Arrays;
import java.util.HashSet;

import org.ff4j.property.PropertyLogLevel.LogLevel;

/**
 * Custom property to code a logLevel.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class PropertyLogLevel extends Property<LogLevel> {

    /** Serial. */
    private static final long serialVersionUID = 1792311055570779010L;

    /** Expected Log Levels. */
    public static enum LogLevel {TRACE, DEBUG, INFO, WARN, ERROR, FATAL}
    
    /**
     * Constructor by string expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      current log level
     */
    public PropertyLogLevel() {
    }
    
    /**
     * Constructor by string expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      current log level
     */
    public PropertyLogLevel(String uid) {
       super(uid);
    }
    
    /**
     * Constructor by string expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      current log level
     */
    public PropertyLogLevel(String uid, String lvl) {
       super(uid, lvl);
       setFixedValues(new HashSet<LogLevel>(Arrays.asList(LogLevel.values())));
    }
    
    /**
     * Constructor by enum expression.
     *
     * @param uid
     *      unique name
     * @param lvlv
     *      current log level
     */
    public PropertyLogLevel(String uid, LogLevel lvl) {
        super(uid, lvl, LogLevel.values());
    }
    
    /** {@inheritDoc} */
    @Override
    public LogLevel fromString(String v) {
        return LogLevel.valueOf(v);
    } 
    
    /**
     * update to trace
     */
    public void trace() {
        setValue(LogLevel.TRACE);
    }

    /**
     * update to debug
     */
    public void debug() {
        setValue(LogLevel.DEBUG);
    }  

    /**
     * update to ingo
     */
    public void info() {
        setValue(LogLevel.INFO);
    }  

    /**
     * update to warn
     */
    public void warn() {
        setValue(LogLevel.WARN);
    }  

    /**
     * update to error
     */
    public void error() {
        setValue(LogLevel.ERROR);
    }  

    /**
     * update to fatal
     */
    public void fatal() {
        setValue(LogLevel.FATAL);
    }
       
}
