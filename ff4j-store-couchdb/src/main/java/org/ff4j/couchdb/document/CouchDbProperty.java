package org.ff4j.couchdb.document;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.NonNull;
import org.ektorp.support.CouchDbDocument;

/*
 * #%L
 * ff4j-store-couchdb
 * %%
 * Copyright (C) 2013 - 2019 FF4J
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

@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class CouchDbProperty extends CouchDbDocument {

    private static final long serialVersionUID = 6103055129012966805L;
    
    @NonNull
    private String type;
    private String property;
}
