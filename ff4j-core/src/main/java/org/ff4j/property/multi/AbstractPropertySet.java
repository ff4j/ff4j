package org.ff4j.property.multi;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2015 FF4J
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


import java.util.HashSet;
import java.util.Set;

/**
 * SuperClass for property as lists.
 *
 * @author Cedrick Lunven (@clunven)
 *
 * @param <T>
 *      current type
 */
public abstract class AbstractPropertySet < T > extends AbstractPropertyMultiValued< T, Set <T>> implements Set < T > {

    /** Serial. */
    private static final long serialVersionUID = 4064427839404299895L;

    /**
     * Default constructor.
     */
    public AbstractPropertySet() {
    }
    
    /**
     * Constructor by property name.
     *
     * @param name
     *      property name
     */
    public AbstractPropertySet(String name) {
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
    public AbstractPropertySet(String uid, Set <T> value) {
       super(uid, value);
    }
    
    /**
     * Constructor by string expression.
     *
     * @param uid
     *      unique name
     * @param lvl
     *      current double value
     */
    public AbstractPropertySet(String uid, String value) {
       super(uid, value);
    }
    
    /** {@inheritDoc} */
    @Override
    public Set<T> fromString(String v) {
        return new HashSet<T>(super.fromString(v));
    }

}
