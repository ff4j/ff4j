package org.ff4j.feature.togglestrategy;

import static org.ff4j.utils.JsonUtils.collectionAsJson;
import static org.ff4j.utils.JsonUtils.valueAsJson;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.ff4j.feature.exception.InvalidStrategyTypeException;
import org.ff4j.property.Property;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 Ff4J
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

/**
 * Each feature should implement the flipping strategy. (enabling/disabling will be handle by flipper.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public interface TogglePredicate extends Predicate< ToggleContext > {
    
    /**
     * Predicate with 2 params
     * 
     * @param ctx
     *          toggle context
     * @return
     *      if the feature would be toggled or not
     */
    default boolean test() {
        return test(null);
    }
            
    /**
     * Predicate with 2 params
     * 
     * @param ctx
     *          toggle context
     * @return
     *      if the feature would be toggled or not
     */
    boolean test(ToggleContext ctx);
   
    /**
     * Initialize Toggle predicate with some parameters.
     *
     * @param uid
     *          feature identifier
     * @param params
     *          set of metadata
     */
    void init(String uid, Set<Property<?>> params);
    
    /**
     * Syntaxic Sugar.
     */
    default void init(String uid, Property<?>... params) {
        if (params != null) {
            init(uid, new HashSet<Property<?>>(Arrays.asList(params)));
        }
    }
    
    /**
     * Initial Parameters required to insert this new flipping.
     * 
     * @return initial parameters for this strategy
     */
    Stream<Property<?>> getProperties();
    
    /**
     * Syntaxic Sugar : Return properties as a MAP.
     * 
     * @return
     *      map with properties
     */
    default Map < String, Property<?>> getPropertiesAsMap() {
        Stream<Property<?>> props = getProperties();
        if (props == null) return null;
        return props.collect(Collectors.toMap(Property::getUid, Function.identity()));
    }
    
    /**
     * To be used on the Json generation.
     *
     * @return
     *      current implementation
     */
    default String getClassName() {
        return getClass().getName();
    }
    
    /**
     * Generate flipping strategy as json.
     * 
     * @return
     *      flippling strategy as json.     
     */
     default String toJson() {
        StringBuilder json = new StringBuilder("{");
        json.append(valueAsJson("properties") + ":");
        Stream<Property<?>> properties = getProperties();
        if (properties == null) {
            json.append("null");
        } else {
            json.append(collectionAsJson(properties.collect(Collectors.toList())));
        }
        json.append("," + valueAsJson("className")  + ":");
        json.append(valueAsJson(getClass().getCanonicalName()));
        json.append("}");
        return json.toString();
    }
     
    /**
     * Instanciate flipping strategy from its class name.
     *
     * @param className
     *      current class name
     * @return
     *      the flipping strategy
     */
    public static TogglePredicate of(String uid, String className, Set <Property<?>> params) {
        try {
            TogglePredicate strategy = (TogglePredicate) Class.forName(className).newInstance();
            strategy.init(uid, params);
            return strategy;
        } catch (Exception ie) {
            throw new InvalidStrategyTypeException(className, ie);
        }
    }
    
    public static TogglePredicate of(String uid, String className,  Stream<Property<?>> params) {
        return of(uid, className, (params != null) ? params.collect(Collectors.toSet()) : new HashSet<>());
    }
    
}
