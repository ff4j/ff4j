package org.ff4j.property;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2018 FF4J
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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.ParameterizedType;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Enable Multivalued properties. 
 *
 * @author Cedrick LUNVEN (@clunven)
 *
 * @param <T>
 *      basic type (int, short....)
 * @param <K>
 * `    simple property type using associated to same basic ex. String and PropertyString.
 */
public abstract class PropertySet<T, K extends Property<T>> extends Property < Set < T> > {

    /** Serialisation. */
    private static final long serialVersionUID = 7171347992502786427L;
    
    /** Instance of simple property to use `fromString()` on each element of the list. */
    private K property;
    
    /** Use the delimiter of lists. */
    protected String listDelimiter = ",";

    /**
     * Constructors leveraging Property<List<X>> and initializing Property<X>
     *
     * @param uid
     *      property ID
     */
    public PropertySet(String uid) {
        super(uid);
        initProperty(uid);
    }
    public PropertySet(String uid, String valueAsString) {
        super(uid, valueAsString);
        initProperty(uid);
    }
    public PropertySet(String uid, Set<T> value) {
        super(uid, value);
        initProperty(uid);
    }
    @SuppressWarnings("unchecked")
    public PropertySet(String uid, T... value) {
        super(uid, new LinkedHashSet<T>(Arrays.asList(value)));
        initProperty(uid);
    }
    
    /** {@inheritDoc} */
    @Override
    public Set<T> fromString(String v) {
        if (v == null) return null;
        return Arrays.stream(v.split(listDelimiter))
                     .map(String::trim)
                     .map(property::fromString)
                     .collect(Collectors.toSet());
    }
    
    /**
     * Serialized value as String
     *
     * @return current value as a string or null
     */
    public String asString() {
        if (get() == null) {
            return null;
        }
        // Handle Collections
        if (Collection.class.isAssignableFrom(get().getClass())) {
           Collection<T> collection = (Collection<T>) get();
           if (collection == null || collection.isEmpty()) {
               return "";
           }
           Iterator<T> it = collection.iterator();
           StringBuilder sb = new StringBuilder(it.next().toString());
           while (it.hasNext()) {
               sb.append(listDelimiter);
               sb.append(it.next());
           }
           return sb.toString();
        }
        return get().toString();
    }
    
    @SuppressWarnings("unchecked")
    protected void initProperty(String uid) {
        try {
            Class<K> persistentClass = (Class<K>)
                    ((ParameterizedType) getClass().getGenericSuperclass()).getActualTypeArguments()[1];
            property = persistentClass.getConstructor(String.class).newInstance(uid);
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException
                | NoSuchMethodException | SecurityException e) {
            throw new IllegalArgumentException("Cannot use reflection to instanciate Generic type :"
                    + "constructor with single argument string should exist", e);
        }
    }    

    /**
     * Getter accessor for attribute 'listDelimiter'.
     *
     * @return
     *       current value of 'listDelimiter'
     */
    public String getListDelimiter() {
        return listDelimiter;
    }

    /**
     * Setter accessor for attribute 'listDelimiter'.
     * @param listDelimiter
     *      new value for 'listDelimiter '
     */
    public void setListDelimiter(String listDelimiter) {
        this.listDelimiter = listDelimiter;
    }
    
}
