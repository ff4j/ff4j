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


import java.util.List;

/**
 * Specialization of {@link AbstractPropertyList} using Integer.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class PropertyListInt extends AbstractPropertyList < Integer > {

    /** Serial. */
    private static final long serialVersionUID = 2044668915134536364L;
    
    /**
     * Default constructor.
     */
    public PropertyListInt() {
    }
    
    /**
     * Constructor by property name.
     *
     * @param name
     *      property name
     */
    public PropertyListInt(String name) {
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
    public PropertyListInt(String uid, String value) {
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
    public PropertyListInt(String uid, List<Integer> value) {
       super(uid, value);
    }
    
    /** {@inheritDoc} */
    @Override
    public  List<Integer> fromString(String v) {
        return super.fromString(v);
    }


}
