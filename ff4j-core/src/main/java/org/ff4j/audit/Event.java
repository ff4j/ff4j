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
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

import org.ff4j.utils.IOUtil;

/**
 * Audit information relevant to features.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class Event implements Serializable, Comparable < Event > {

    /** Serial. */
    private static final long serialVersionUID = 6490780530212257217L;

    /** Unique identifier. */
    private String uuid;
    
    /** Time of event creation. */
    private long timestamp;
    
    /** Duration of action. */
    private long duration = 0;
    
    /** HostName. */
    private String hostName;
    
    /** Source. */
    private String source;
    
    /** Current user. */
    private String user;
    
    /** feature or property name. */
    private String name;
    
    /** feature or property. */
    private String type;
     
    /** Action performed. */
    private String action;
    
    /** Common element. */
    private String value;
    
    /** Specific parameters. */
    private Map < String, String > customKeys = new HashMap<String, String>();
    
    /**
     * Default constructor.
     * 
     */
    public Event() {
        uuid        = UUID.randomUUID().toString();
        timestamp   = System.currentTimeMillis();
        hostName    = IOUtil.resolveHostName();
    }
    
    /** Default constructor. */
    public Event(String pSource, String pType, String pName, String pAction) {
        this();
        this.source = pSource;
        this.type   = pType;
        this.name   = pName;
        this.action = pAction;
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return toJson();
    }
    
    /**
     * Serialized as a Json document.
     *
     * @return
     *      current evetn as CSV item
     */
    public String toJson() {
        StringBuilder sb = new StringBuilder("{");
        sb.append("\"id\": \"" + uuid + "\"");
        sb.append(", \"timestamp\":" + timestamp);
        keyAsString(sb, "hostName", hostName);
        keyAsString(sb, "source",   source);
        keyAsString(sb, "user",   user);
        keyAsString(sb, "name",   name);
        keyAsString(sb, "type",   type);
        keyAsString(sb, "action", action);
        keyAsString(sb, "value", value);
        sb.append(", \"duration\":" + duration);
        if (customKeys != null && !customKeys.isEmpty()) {
            for(Map.Entry<String,String> customKeysEntry : customKeys.entrySet()) {
                if (null != customKeysEntry.getValue()) {
                    keyAsString(sb, customKeysEntry.getKey(), customKeysEntry.getValue());
                }
            }
        }
        sb.append("}");
        return sb.toString();
    }
    
    /**
     * Add custom key to event.
     *
     * @param key
     *      custom key
     * @param value
     *      current value
     */
    public void put(String key, String value) {
        getCustomKeys().put(key, value);
    }
    
    /**
     * 
     * @param key
     * @return
     */
    public String getKey(String key) {
        return getCustomKeys().get(key);
    }
    
    /**
     * Add key to Json expression.
     *
     * @param sb
     *      current output
     * @param name
     *      current key
     * @param value
     *      current value
     */
    private void keyAsString(StringBuilder sb, String name, String value) {
        if (value != null) {
            sb.append(", \"" + name + "\": \"" + value + "\"");
        }
    }
    
    /**
     * Getter accessor for attribute 'timestamp'.
     * 
     * @return current value of 'timestamp'
     */
    public long getTimestamp() {
        return timestamp;
    }
    
    /**
     * Getter accessor for attribute 'timestamp'.
     * 
     * @return current value of 'timestamp'
     */
    public Date getDate() {
        return new Date(getTimestamp());
    }
    
    /**
     * Getter accessor for attribute 'uuid'.
     *
     * @return
     *       current value of 'uuid'
     */
    public String getUuid() {
        return uuid;
    }

    /**
     * Setter accessor for attribute 'uuid'.
     * @param uuid
     * 		new value for 'uuid '
     */
    public void setUuid(String uuid) {
        this.uuid = uuid;
    }

    /**
     * Getter accessor for attribute 'hostName'.
     *
     * @return
     *       current value of 'hostName'
     */
    public String getHostName() {
        return hostName;
    }

    /**
     * Setter accessor for attribute 'hostName'.
     * @param hostName
     * 		new value for 'hostName '
     */
    public void setHostName(String hostName) {
        this.hostName = hostName;
    }

    /**
     * Getter accessor for attribute 'source'.
     *
     * @return
     *       current value of 'source'
     */
    public String getSource() {
        return source;
    }

    /**
     * Setter accessor for attribute 'source'.
     * @param source
     * 		new value for 'source '
     */
    public void setSource(String source) {
        this.source = source;
    }

    /**
     * Getter accessor for attribute 'user'.
     *
     * @return
     *       current value of 'user'
     */
    public String getUser() {
        return user;
    }

    /**
     * Setter accessor for attribute 'user'.
     * @param user
     * 		new value for 'user '
     */
    public void setUser(String user) {
        this.user = user;
    }

    /**
     * Getter accessor for attribute 'name'.
     *
     * @return
     *       current value of 'name'
     */
    public String getName() {
        return name;
    }

    /**
     * Setter accessor for attribute 'name'.
     * @param name
     * 		new value for 'name '
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * Getter accessor for attribute 'type'.
     *
     * @return
     *       current value of 'type'
     */
    public String getType() {
        return type;
    }

    /**
     * Setter accessor for attribute 'type'.
     * @param type
     * 		new value for 'type '
     */
    public void setType(String type) {
        this.type = type;
    }
    
    /**
     * Getter accessor for attribute 'action'.
     *
     * @return
     *       current value of 'action'
     */
    public String getAction() {
        return action;
    }

    /**
     * Setter accessor for attribute 'action'.
     * @param action
     * 		new value for 'action '
     */
    public void setAction(String action) {
        this.action = action;
    }

    /**
     * Getter accessor for attribute 'customKeys'.
     *
     * @return
     *       current value of 'customKeys'
     */
    public Map<String, String> getCustomKeys() {
        return customKeys;
    }

    /**
     * Setter accessor for attribute 'customKeys'.
     * @param customKeys
     * 		new value for 'customKeys '
     */
    public void setCustomKeys(Map<String, String> customKeys) {
        this.customKeys = customKeys;
    }

    /**
     * Setter accessor for attribute 'timestamp'.
     * @param timestamp
     * 		new value for 'timestamp '
     */
    public void setTimestamp(long timestamp) {
        this.timestamp = timestamp;
    }

    /**
     * Getter accessor for attribute 'duration'.
     *
     * @return
     *       current value of 'duration'
     */
    public long getDuration() {
        return duration;
    }

    /**
     * Setter accessor for attribute 'duration'.
     * @param duration
     * 		new value for 'duration '
     */
    public void setDuration(long duration) {
        this.duration = duration;
    }

    /**
     * Getter accessor for attribute 'value'.
     *
     * @return
     *       current value of 'value'
     */
    public String getValue() {
        return value;
    }

    /**
     * Setter accessor for attribute 'value'.
     * @param value
     * 		new value for 'value '
     */
    public void setValue(String value) {
        this.value = value;
    }

    /** {@inheritDoc} */
    @Override
    public int compareTo(Event evt) {
        int myTime = new Long(this.getTimestamp() - evt.getTimestamp()).intValue();
        // Not equals even if same timestamp (of course...)
        return (myTime != 0) ? myTime : evt.getUuid().compareTo(getUuid());
    }

}
