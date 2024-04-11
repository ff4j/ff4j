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
import org.ektorp.support.TypeDiscriminator;

@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class CouchDbFeature extends CouchDbDocument {

    private static final long serialVersionUID = -395071089004600797L;
    
    @NonNull
    @TypeDiscriminator
    private String type;
    
    private String feature;

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
     * Getter accessor for attribute 'feature'.
     *
     * @return
     *       current value of 'feature'
     */
    public String getFeature() {
        return feature;
    }

    /**
     * Setter accessor for attribute 'feature'.
     * @param feature
     * 		new value for 'feature '
     */
    public void setFeature(String feature) {
        this.feature = feature;
    }
}
