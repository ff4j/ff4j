package org.ff4j.audit;

/*
 * #%L
 * ff4j-core
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

import org.ff4j.FF4j;

import static org.ff4j.audit.EventConstants.*;

public class EventBuilder {
    
    /** API Configuration. */
    private Event event;
    
    /**
     * Default constructor.
     */
    public EventBuilder() {
        this.event = new Event();
    }
    
    /**
     * Constructor with ff4J.
     *
     * @param ff4j
     */
    public EventBuilder(FF4j ff4j) {
        this();
        
        // Source can be WEB, SSH, JAVA etc
        this.event.setSource(ff4j.getSource());
        
        // Retrieved looged user from AuthorizationManager
        if (ff4j.getAuthorizationsManager() != null) {
            event.setUser(ff4j.getAuthorizationsManager().getCurrentUserName());
        }
    }
    
    public EventBuilder feature(String uid) {
        event.setType(TARGET_FEATURE);
        event.setName(uid);
        return this;
    }
    
    public EventBuilder group(String groupName) {
        event.setType(TARGET_GROUP);
        event.setName(groupName);
        return this;
    }
    
    public EventBuilder property(String name) {
        event.setType(TARGET_PROPERTY);
        event.setName(name);
        return this;
    }
    
    public EventBuilder action(String action) {
        event.setAction(action);
        return this;
    }
    
    public EventBuilder duration(long duration) {
        event.setDuration(duration);
        return this;
    }

    public EventBuilder type(String type) {
        event.setType(type);
        return this;
    }
    
    public EventBuilder value(String val) {
        event.setValue(val);
        return this;
    }
    
    public EventBuilder name(String name) {
        event.setName(name);
        return this;
    }
    
    public Event build() {
        return event;
    }

}
