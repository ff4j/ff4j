package org.ff4j.arangodb.document;

/*-
 * #%L
 * ff4j-store-arangodb
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

import com.arangodb.entity.DocumentField;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.ff4j.core.FlippingStrategy;

import java.util.Map;
import java.util.Set;

/**
 * ArangoDB document for storing {@link org.ff4j.core.Feature} feature
 */
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class ArangoDBFeature {

    /**
     * ArangoDB id
     */
    @DocumentField(DocumentField.Type.KEY)
    private String id;

    /**
     * Feature attributes
     */
    private String uid;
    private boolean enable;
    private String description;
    private String group;
    private Set<String> permissions;
    private FlippingStrategy flippingStrategy;
    private Map<String, ArangoDBProperty> customProperties;
}
