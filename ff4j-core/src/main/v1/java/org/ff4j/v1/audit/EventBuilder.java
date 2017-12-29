package org.ff4j.v1.audit;

import static org.ff4j.v1.audit.EventConstants.*;

import org.ff4j.v1.FF4j;

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
