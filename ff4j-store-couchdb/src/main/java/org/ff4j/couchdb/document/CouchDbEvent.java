package org.ff4j.couchdb.document;

/*-
 * #%L
 * ff4j-store-couchdb
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.ektorp.support.CouchDbDocument;

@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class CouchDbEvent extends CouchDbDocument {

    private static final long serialVersionUID = -5807287720388921887L;

    @NonNull
    private String type;
    
    private String event;

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
     * Getter accessor for attribute 'event'.
     *
     * @return
     *       current value of 'event'
     */
    public String getEvent() {
        return event;
    }

    /**
     * Setter accessor for attribute 'event'.
     * @param event
     * 		new value for 'event '
     */
    public void setEvent(String event) {
        this.event = event;
    }


}
