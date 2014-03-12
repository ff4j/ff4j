package org.ff4j.audit;

import java.io.Serializable;

/**
 * Audit information relevant to features.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class Event implements Serializable {

    /** Serial. */
    private static final long serialVersionUID = 6490780530212257217L;

    /** Type of event. */
    private EventType type;

    /** Target Feature. */
    private String featureName;

    /** Time of event creation. */
    private final long timestamp;

    /** Default constructor. */
    public Event() {
        timestamp = System.currentTimeMillis();
    }

    /**
     * Paramterized constructor.
     * 
     * @param featureName
     *            target feature name
     * @param type
     *            target event type
     */
    public Event(String featureName, EventType type) {
        this();
        this.featureName = featureName;
        this.type = type;
    }

    /**
     * Getter accessor for attribute 'type'.
     * 
     * @return current value of 'type'
     */
    public EventType getType() {
        return type;
    }

    /**
     * Setter accessor for attribute 'type'.
     * 
     * @param type
     *            new value for 'type '
     */
    public void setType(EventType type) {
        this.type = type;
    }

    /**
     * Getter accessor for attribute 'featureName'.
     * 
     * @return current value of 'featureName'
     */
    public String getFeatureName() {
        return featureName;
    }

    /**
     * Setter accessor for attribute 'featureName'.
     * 
     * @param featureName
     *            new value for 'featureName '
     */
    public void setFeatureName(String featureName) {
        this.featureName = featureName;
    }

    /**
     * Getter accessor for attribute 'timestamp'.
     * 
     * @return current value of 'timestamp'
     */
    public long getTimestamp() {
        return timestamp;
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return "Event [type=" + type + ", featureName=" + featureName + ", timestamp=" + timestamp + "]";
    }

}
