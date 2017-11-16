package org.ff4j.couchbase.store.document;

/*
 * #%L
 * ff4j-store-springcouchbase
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

import com.couchbase.client.java.repository.annotation.Field;
import com.couchbase.client.java.repository.annotation.Id;
import lombok.Data;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.property.Property;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * Created by farrellyja on 09/11/2017.
 */
@Data
public class FeatureDocument {
    @Id
    private String uid;
    @Field
    private Boolean enable;
    @Field
    private String description;
    @Field
    private String group;
    @Field
    private Set<String> permissions;
    @Field
    private FlippingStrategy flippingStrategy;
    @Field
    private Map<String, Property<?>> customProperties = new HashMap<>();
}
