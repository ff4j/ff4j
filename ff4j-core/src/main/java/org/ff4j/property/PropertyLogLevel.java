package org.ff4j.property;

import org.ff4j.property.PropertyLogLevel.LogLevel;
import org.ff4j.property.serialize.LogLevelSerializer;
import org.ff4j.property.serialize.Serializer;

/**
 * Custom property to code a logLevel.
 */
public class PropertyLogLevel extends Property<LogLevel> {

    /**
     * String serializer.
     */
    private static final Serializer<LogLevel> SERIALIZER = new LogLevelSerializer();

    /** Expected Log Levels. */
    public enum LogLevel {TRACE, DEBUG, INFO, WARN, ERROR, FATAL}
    
    /**
     * Defaulting to 'INFO'
     *
     * @param uid
     *      unique name
     */
    public PropertyLogLevel(String uid) {
        this(uid, LogLevel.INFO);
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
     * @param lvl
     *      current log level
     */
    public PropertyLogLevel(String uid, LogLevel lvl) {
        super(uid, lvl);
        setFixedValues(LogLevel.values());
    }

    /** {@inheritDoc} */
    @Override
    public Serializer<LogLevel> getSerializer() {
        return SERIALIZER;
    }

    /**
     * update to trace
     */
    public PropertyLogLevel trace() {
        setValue(LogLevel.TRACE);
        return this;
    }

    /**
     * update to debug
     */
    public PropertyLogLevel debug() {
        setValue(LogLevel.DEBUG);
        return this;
    }  

    /**
     * update to ingo
     */
    public PropertyLogLevel info() {
        setValue(LogLevel.INFO);
        return this;
    }  

    /**
     * update to warn
     */
    public PropertyLogLevel warn() {
        setValue(LogLevel.WARN);
        return this;
    }  

    /**
     * update to error
     */
    public PropertyLogLevel error() {
        setValue(LogLevel.ERROR);
        return this;
    }  

    /**
     * update to fatal
     */
    public PropertyLogLevel fatal() {
        setValue(LogLevel.FATAL);
        return this;
    }
       
}
