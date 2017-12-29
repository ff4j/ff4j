package org.ff4j.monitoring;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import java.util.Collection;
import java.util.Optional;
import java.util.stream.Collectors;

import org.ff4j.event.Event;
import org.ff4j.event.EventSeries;

public class AuditTrailQuery {
    
    private Long from;
    
    private Long to;
    
    private Event.Scope scope;
    
    private String uid;
    
    public AuditTrailQuery() {
    }
    
    public AuditTrailQuery from(long from) {
        this.from = new Long(from);
        return this;
    }
    
    public AuditTrailQuery scope(Event.Scope scope) {
        this.scope = scope;
        return this;
    }
    
    public AuditTrailQuery uid(String uid) {
        this.uid = uid;
        return this;
    }
    
    public Optional < Long > getLowerBound() {
        return Optional.ofNullable(from);
    }
    
    public Optional < Long > getUpperBound() {
        return Optional.ofNullable(to);
    }
    
    public Optional < String > getUid() {
        return Optional.ofNullable(uid);
    }
    
    public Optional < Event.Scope > getScope() {
        return Optional.ofNullable(scope);
    }
    
    /**
     * Lisibility, lisibility
     */
    public boolean match(Event evt) {
               // LowerBound
        return ((from == null) || (from != null && evt.getTimestamp() >= from)) &&
               // UpperBound
               ((to == null) || (to != null && evt.getTimestamp() <= to)) &&
               // Scope
               ((scope == null) || scope.name().equalsIgnoreCase(evt.getScope())) &&
               // Uid
               ((uid == null) || uid.equalsIgnoreCase(evt.getTargetUid()));
    }
    
    public Collection < Event > filter(EventSeries es) {
        return es.stream().filter(this::match).collect(Collectors.toList());
    }

}
