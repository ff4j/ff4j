package org.ff4j.event.repository;

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

import java.util.stream.Stream;

import org.ff4j.event.Event;
import org.ff4j.event.monitoring.AuditTrailQuery;

/**
 * Audit Trail is READ ONLY.
 *
 * @author Cedrick LUNVEN  (@clunven)
 */
public interface EventAuditTrailRepository {
    
    /**
     * Create tables related to Audit Trail.
     */
    void createSchema();
    
    /**
     * Insert new event in the DB.
     *
     * @param evt
     *      current event
     */
    void log(Event evt);
    
    /**
     * Search events in the audit trail.
     *
     * @param query
     *      target query
     * @return
     */
    Stream < Event > search(AuditTrailQuery query);
    
    /**
     * Will delete log record matching the query. Will create a new record line to notify.
     *
     * @param query
     *      current query
     */
    void purge(AuditTrailQuery query);
}
