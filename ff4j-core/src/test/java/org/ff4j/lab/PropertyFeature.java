package org.ff4j.lab;

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

import org.ff4j.core.Feature;
import org.ff4j.property.Property;

/**
 * {@link Feature} could be embedded into a property to leverage on property store.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyFeature extends Property < Feature > {
    
    /** serialVersionUID. */
    private static final long serialVersionUID = -2652848439723919709L;
   
    /**
     * Default constructor.
     */
    public PropertyFeature() {
    }
    
    /**
     * Constructor by property name.
     *
     * @param name
     *      property name
     */
    public PropertyFeature(String name) {
        super(name);
    }
    
    /**
     * Constructor by string expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      current double value
     */
    public PropertyFeature(String uid, String value) {
       super(uid, value);
    }
    
    /**
     * Constructor by T expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      current double value
     */
    public PropertyFeature(Feature feat) {
       super(feat.getUid(), feat);
    }
    
    /**
     * Constructor by T expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      current double value
     */
    public PropertyFeature(String uid, Feature feat) {
       super(uid, feat);
    }
    
    /** {@inheritDoc} */
    @Override
    public Feature fromString(String v) {
        // TODO use java8 to parse JSON (no dependency allowed here)
        return new Feature(v);
    }
    
}
