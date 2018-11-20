
package org.ff4j.utils;

import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.net.InetAddress;
import java.net.UnknownHostException;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Optional;
import java.util.Scanner;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.lang.model.type.NullType;

import org.ff4j.FF4jEntity;
import org.ff4j.event.Event;
import org.ff4j.test.AssertUtils;

/**
 * Tips and tricks to be less verbose.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class Util {
    
    private Util() {}

    /**
     * Find a feature or property in a stream
     * @param stream
     * @param uid
     * @return
     */
    public static < T extends FF4jEntity<?> > Optional<T> find(Stream <T> stream, String uid) {
        if (stream == null || uid == null) return Optional.empty();
        return stream.filter(t -> uid.equals(t.getUid())).findFirst();
    }
    
    /**
     * Retrieve all entities as a MAP and not stream collecting by UID.
    *
    * @return entities as an {@link Iterable}
    */
    public static < T extends FF4jEntity<?> > Map < String, T > toMap(Stream<T> stream) {
       if (stream == null) return new HashMap<>(); 
       return stream.collect(Collectors.toMap(T::getUid, Function.identity()));
   } 
    
    /**
     * Create an HashSet.
     *
     * @param els
     *            enumeration of elements
     * @return
     */
    @SuppressWarnings("unchecked")
    public static <T> Set<T> setOf(T... els) {
         return (els == null) ? null : new HashSet<T>(Arrays.asList(els));
    }
    
    /**
     * Creation of a map from a single Value.
     * @param key
     *      map key
     * @param value
     *      map value
     * @return
     *      the populated map
     */
    public static <K,V> Map < K, V > mapOf(K key, V value) {
        Map <K, V> mapOfValues = new HashMap<>();
        mapOfValues.put(key, value);
        return mapOfValues;
    }
    
    /**
     * Create an HashSet.
     *
     * @param els
     *            enumeration of elements
     * @return
     */
    public static <T> Set<T> setOf(Stream < T > elements) {
        return (elements == null) ? null : elements.collect(Collectors.toSet());
    }
    
    /**
     * Create an HashSet.
     *
     * @param els
     *            enumeration of elements
     * @return
     */
    @SuppressWarnings("unchecked")
    public static <T> List<T> listOf(T... els) {
        return (els == null) ? null : new ArrayList<T>(Arrays.asList(els));
    }
    
    /**
     * Check that expression is true.
     * 
     * @param expression
     *            expression to evaluate
     */
    public static boolean hasLength(String expression) {
        return expression != null && !"".equals(expression);
    }
    
    /**
     * Check that class is valid.
     * 
     * @param expression
     *            expression to evaluate
     */
    public static boolean isValidClass(Class<?> clazz) {
        return (clazz != null) && (clazz != NullType.class);
    }
    
    /**
     * Validate event.
     *
     * @param evt
     *          event
     */
    public static void validateEvent(Event evt) {
        AssertUtils.assertNotNull(evt);
        AssertUtils.assertHasLength(evt.getScope());
        AssertUtils.assertHasLength(evt.getTargetUid());
        AssertUtils.assertHasLength(evt.getAction());
    }   
    
    /**
     * Read hostName from JDK.
     * 
     * @return
     *      current hostname
     */
    public static String inetAddressHostName() {
        try {
            return InetAddress.getLocalHost().getHostName();
        } catch (UnknownHostException e) {
            throw new IllegalArgumentException("Cannot find the target host by itself", e);
        }
    }   
    
    /**
     * Serialize collection elements with a delimiter.
     *
     * @param collec
     *      collection (a,b,c)
     * @param delimiter
     *      delimiter char (e.g : ",")
     * @return
     *      the list : a,b,c
     */
    public static <T> String join(Collection < T > collec, String delimiter) {
        AssertUtils.assertHasLength(delimiter);
        if (collec == null) return null;
        StringBuilder sb = new StringBuilder();
        boolean first = true;
        for (T t : collec) {
            if (!first) {
                sb.append(delimiter);
            }
            sb.append(t.toString());
            first = false;
        }
        return sb.toString();
    }
    
    /**
     * Check if a current class can be cast to collection.
     * 
     * @param c
     *      current class
     * @return
     *      flag if it's a collection
     */
    public static boolean isClassCollection(Class<?> c) {
        return Collection.class.isAssignableFrom(c) || Map.class.isAssignableFrom(c);
    }
    
    /**
     * Check if a current object can be cast to collection.
     * 
     * @param ob
     *      current object
     * @return
     *      flag if it's a collection
     */
    public static boolean isCollection(Object ob) {
        return ob != null && isClassCollection(ob.getClass());
    }
    
    /**
     * Check if a current object is empty
     * 
     * @param ob
     *      current collection
     * @return
     *      flag if it's an empty collection
     */
    public static boolean isEmpty(Collection<?> ob) {
        return (ob == null) || ob.isEmpty();
    }
    
    /**
     * Downcast as collection or return error.
     *
     * @param ob
     *      target object
     * @return
     *      if can ve converted to collection.
     */
    @SuppressWarnings("unchecked")
    public static <T> Collection < T > asCollection(Object ob) {
        if (ob == null) return null;
        if (ob.getClass().isArray()) {
            return Arrays.asList((T[]) ob);
        }
        if (!isCollection(ob)) {
            throw new IllegalArgumentException("Target Object is not collection");
        }
        return (Collection<T>) ob;
    }
    
    /**
     * Get a random offset within map.
     *
     * @param size
     *      target list size
     * @return
     *      a random positive integer below size
     */
    public static int getRandomOffset(int size) {
        return (int) (Math.random() * Math.abs(size));
    }
    
    /**
     * Get a random element from a list.
     *
     * @param myList
     *      current list
     */
    public static < T > T getRandomElement(List<T> myList) {
        return myList.get(getRandomOffset(myList.size()));
    }
     
    /**
     * Allow to instanciate utility class.
     *
     * @param utilityClass
     *      current utility
     * @return
     *      instance
     * @throws Exception
     */
    public static <T> T instanciatePrivate(Class<T> utilityClass) throws Exception {
        Constructor<T> ce = utilityClass.getDeclaredConstructor();
        ce.setAccessible(true);
        return ce.newInstance();
    }
    
    /**
     * Get key listfrom value.
     *
     * @param map
     *      current MAP
     * @param value
     *      value of MAP
     * @return
     */
    public static <T, E> Set<T> getKeysByValue(Map<T, E> map, E value) {
        if (map == null) return null;
        Set<T> keys = new HashSet<T>();
        for (Entry<T, E> entry : map.entrySet()) {
            if (value != null && value.equals(entry.getValue())) {
                keys.add(entry.getKey());
            }
        }
        return keys;
    }
    
    /**
     * Get a first key matching from value.
     *
     * @param map
     *      current MAP
     * @param value
     *      value of MAP
     * @return
     */
    public static <T, E> T getFirstKeyByValue(Map<T, E> map, E value) {
        if (map == null) return null;
        for (Entry<T, E> entry : map.entrySet()) {
            if (value != null && value.equals(entry.getValue())) {
                return entry.getKey();
            }
        }
        return null;
    }
    
    public static String fromInputStreamToString(InputStream in) {
        try(Scanner scan = new Scanner(in)) { 
            return scan.useDelimiter("\\A").hasNext() ? scan.next() : "";
        }
    }

}
