package org.ff4j.audit;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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
    private long timestamp;

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
     * Paramterized constructor.
     * 
     * @param featureName
     *            target feature name
     * @param type
     *            target event type
     */
    public Event(String featureName, EventType type, long time) {
        this(featureName, type);
        this.timestamp = time;
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
        return toJson();
    }
    
    /**
     * Serialized as a CSV line (without line return).
     *
     * @return
     *      current evetn as CSV item
     */
    public String toCSV() {
        return timestamp + ";" + featureName + ";" + type;
    }
    
    
    /**
     * Serialized as a CSV line (without line return).
     *
     * @return
     *      current evetn as CSV item
     */
    public String toThreadName() {
        return timestamp + "-" + featureName + "-" + type;
    }
    
    /**
     * Serialized as a Json document.
     *
     * @return
     *      current evetn as CSV item
     */
    public String toJson() {
        return "{\"type\": \"" + type + "\", \"featureName\":\"" + featureName + "\", \"timestamp\":" + timestamp + "}";
    }

}
