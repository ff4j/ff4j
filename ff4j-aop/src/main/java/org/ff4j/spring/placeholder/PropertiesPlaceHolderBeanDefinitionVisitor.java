package org.ff4j.spring.placeholder;

/*
 * #%L
 * ff4j-aop
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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


import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.Property;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanDefinitionStoreException;
import org.springframework.beans.factory.config.BeanDefinitionVisitor;

/**
 * Pattern Bean Visitor.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class PropertiesPlaceHolderBeanDefinitionVisitor extends BeanDefinitionVisitor {
    
    /** Static commons-log LOG for this class. * */
    private static final Logger LOGGER = LoggerFactory.getLogger(PropertiesPlaceHolderBeanDefinitionVisitor.class);
    
    /** Properties Map from store **/
    private Map<String, Property<?>> propertiesMap = new HashMap<String, Property<?>>();
    
    /** Properties Map from store **/
    private Map<String, Feature > featuresMap = new HashMap<String, Feature >();
    
    /** Prefix to every registry stored adress. * */
    public static final String PLACEHOLDER_PROPERTY_PREFIX = "@ff4jProperty{";
    
    /** Prefix to every registry stored adress. * */
    public static final String PLACEHOLDER_FEATURE_PREFIX = "@ff4jFeature{";

    /** Prefix to every registry stored adress. * */
    public static final String PLACEHOLDER_SUFFIX = "}";
    
    /**
     * Instanciate visitor.
     * @param serviceMap
     *      parameter service map
     */
    public PropertiesPlaceHolderBeanDefinitionVisitor(FF4j ff4j) {
        if (ff4j == null) {
           throw new IllegalArgumentException("Cannot initialize placeholding 'ff4j' is null");
        }       
        this.propertiesMap = ff4j.getProperties();
        this.featuresMap   = ff4j.getFeatures();
    }
    
    /** {@inheritDoc} */
    protected String resolveStringValue(String strVal) throws BeansException {
        return parseStringValue(strVal, propertiesMap, featuresMap, new HashSet<String>());
    }
    
    /**
     * Parsing value to handle
     * @param strVal
     * @param uriMap
     * @param visitedPlaceholders
     * @return
     * @throws BeanDefinitionStoreException
     */
    protected String parseStringValue(String strVal, Map<String, Property<?>> propertiesMap, Map<String, Feature> featureMap, Set<String> visitedPlaceholders) 
    throws BeanDefinitionStoreException {
        StringBuilder builder = new StringBuilder(strVal);
        
        // @ff4jProperty{}
        int startIndex = strVal.indexOf(PLACEHOLDER_PROPERTY_PREFIX);
        while (startIndex != -1) {
            int endIndex = builder.toString().indexOf(PLACEHOLDER_SUFFIX, startIndex + PLACEHOLDER_PROPERTY_PREFIX.length());
            if (endIndex != -1) {
                String placeholder = builder.substring(startIndex + PLACEHOLDER_PROPERTY_PREFIX.length(), endIndex);
                if (!visitedPlaceholders.add(placeholder)) {
                    throw new BeanDefinitionStoreException("Circular placeholder reference '" + placeholder + "' in property definitions");
                }
                if (propertiesMap==null || !propertiesMap.containsKey(placeholder)) {
                    throw new PropertyNotFoundException(
                            PLACEHOLDER_PROPERTY_PREFIX + ": Cannot perform placeholding on " + placeholder);
                }
                String propVal = propertiesMap.get(placeholder).asString();
                if (propVal != null) {
                    propVal = parseStringValue(propVal, propertiesMap, featureMap, visitedPlaceholders);
                    builder.replace(startIndex, endIndex + PLACEHOLDER_SUFFIX.length(), propVal);
                    if (LOGGER.isDebugEnabled()) {
                        LOGGER.debug("Resolved placeholder '{}' to value '{}'", placeholder, propVal);
                    }
                    startIndex = builder.toString().indexOf(PLACEHOLDER_PROPERTY_PREFIX, startIndex + propVal.length());
                } else {
                    throw new BeanDefinitionStoreException("Could not resolve placeholder '" + placeholder + "'");
                }
                visitedPlaceholders.remove(placeholder);
            } else {
                startIndex = -1;
            }
        }
        
        // @ff4jFeature{}
        startIndex = strVal.indexOf(PLACEHOLDER_FEATURE_PREFIX);
        while (startIndex != -1) {
            int endIndex = builder.toString().indexOf(PLACEHOLDER_SUFFIX, startIndex + PLACEHOLDER_FEATURE_PREFIX.length());
            if (endIndex != -1) {
                String placeholder = builder.substring(startIndex + PLACEHOLDER_FEATURE_PREFIX.length(), endIndex);
                if (!visitedPlaceholders.add(placeholder)) {
                    throw new BeanDefinitionStoreException("Circular placeholder reference '" + placeholder + "' in property definitions");
                }
                if (featureMap==null || !featureMap.containsKey(placeholder)) {
                    throw new FeatureNotFoundException(
                            PLACEHOLDER_FEATURE_PREFIX + ": Cannot perform placeholding on " + placeholder);
                }
                String propVal = String.valueOf(featureMap.get(placeholder).isEnable());
                if (propVal != null) {
                    propVal = parseStringValue(propVal, propertiesMap, featureMap, visitedPlaceholders);
                    builder.replace(startIndex, endIndex + PLACEHOLDER_FEATURE_PREFIX.length(), propVal);
                    if (LOGGER.isDebugEnabled()) {
                        LOGGER.debug("Resolved placeholder '{}' to value '{}'", placeholder, propVal);
                    }
                    startIndex = builder.toString().indexOf(PLACEHOLDER_FEATURE_PREFIX, startIndex + propVal.length());
                } else {
                    throw new BeanDefinitionStoreException("Could not resolve placeholder '" + placeholder + "'");
                }
                visitedPlaceholders.remove(placeholder);
            } else {
                startIndex = -1;
            }
        }
        
        return builder.toString();
    }
    
}