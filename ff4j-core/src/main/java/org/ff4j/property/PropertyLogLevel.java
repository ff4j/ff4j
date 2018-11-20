package org.ff4j.property;

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
       this(uid, LogLevel.valueOf(lvl));
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
        super(uid, lvl);
        setFixedValues(LogLevel.values());
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
