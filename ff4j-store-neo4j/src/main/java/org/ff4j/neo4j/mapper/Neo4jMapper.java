package org.ff4j.neo4j.mapper;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/*
 * #%L
 * ff4j-store-neo4j
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

import org.ff4j.core.Feature;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyString;
import org.ff4j.property.util.PropertyFactory;
import org.ff4j.utils.MappingUtil;
import org.neo4j.graphdb.Node;

import static org.ff4j.neo4j.FF4jNeo4jConstants.*;

/**
 * Map Neo4j node from and to {@link Feature} bean.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class Neo4jMapper {
    
    /**
     * Hide default constructor.
     */
    private Neo4jMapper() {
    }
    
    /**
     * Transform  node FF4J_FEATURE into core Feature.
     *
     * @param nodeFeature
     * @return
     */
    public static Feature fromNode2Feature(Node nodeFeature) {
        Map < String, Object > nodeProperties = nodeFeature.getAllProperties();
        String featureID    = (String) nodeFeature.getProperty(NODEFEATURE_ATT_UID);
        Boolean enable      = (Boolean) nodeFeature.getProperty(NODEFEATURE_ATT_ENABLE);
        Feature feature = new Feature(featureID, enable);
        if (nodeProperties.containsKey(NODEFEATURE_ATT_DESCRIPTION)) {
            feature.setDescription((String) nodeFeature.getProperty(NODEFEATURE_ATT_DESCRIPTION));
        }
        if (nodeProperties.containsKey(NODEFEATURE_ATT_ROLES)) {
            String[] roles = (String[]) nodeFeature.getProperty(NODEFEATURE_ATT_ROLES);
            feature.setPermissions(new HashSet<>(Arrays.asList(roles)));
        }
        return feature;
    }
    
    /**
     * Mapping from Neo4j node to {@link FlippingStrategy}.
     * 
     * @param featureUid
     *      current feature unique identifier.
     * @param nodeFlippingStrategy
     *      current strategy
     * @return
     *      target flippingStrategy
     */
    static public FlippingStrategy fromNode2FlippingStrategy(String featureUid, Node nodeFlippingStrategy) {
        Map < String, Object > nodeProperties = nodeFlippingStrategy.getAllProperties();
        HashMap < String, String > initParams = new HashMap<>();
        if (nodeProperties.containsKey(NODESTRATEGY_ATT_INITPARAMS)) {
            String[] initParamsExpression = (String[]) nodeFlippingStrategy.getProperty(NODESTRATEGY_ATT_INITPARAMS);
            for (String currentExpression : initParamsExpression) {
                String[] chunks = currentExpression.split("=");
                if (chunks != null && chunks.length > 1) { 
                    initParams.put(chunks[0], chunks[1]);
                }
            }
        }
        String className  = (String) nodeFlippingStrategy.getProperty(NODESTRATEGY_ATT_TYPE);
        return MappingUtil.instanceFlippingStrategy(featureUid, className, initParams);       
    }
    
    
    /**
     * Mapping from Neo4j node to {@link Property}
     * 
     * @param featureUid
     *      unique feature identifier
     * @param nodeProperty
     *      node property
     * @return
     *      value of node
     */
    static public Property<?> fromNode2Property(Node nodeProperty) {
        Map < String, Object > nodeProperties = nodeProperty.getAllProperties();
        if (!nodeProperties.containsKey(NODEPROPERTY_ATT_NAME)) {
            throw new IllegalArgumentException(NODEPROPERTY_ATT_NAME + " is a required property");
        }
        String propertyName  = (String) nodeProperty.getProperty(NODEPROPERTY_ATT_NAME);
        if (!nodeProperties.containsKey(NODEPROPERTY_ATT_VALUE)) {
            throw new IllegalArgumentException(NODEPROPERTY_ATT_VALUE + " is a required property id=" + propertyName);
        }
        String propertyValue = (String) nodeProperty.getProperty(NODEPROPERTY_ATT_VALUE);
        String propertyType  = PropertyString.class.getName();
        if (nodeProperties.containsKey(NODEPROPERTY_ATT_TYPE)) {
            propertyType = (String) nodeProperty.getProperty(NODEPROPERTY_ATT_TYPE);
        }
        String propertyDescription = null;
        if (nodeProperties.containsKey(NODEPROPERTY_ATT_DESCRIPTION)) {
            propertyDescription = (String) nodeProperty.getProperty(NODEPROPERTY_ATT_DESCRIPTION);
        }
        if (nodeProperties.containsKey(NODEPROPERTY_ATT_FIXEDVALUES)) {
            String[] listOfValues = (String[]) nodeProperty.getProperty(NODEPROPERTY_ATT_FIXEDVALUES);
            Set < String > fixedValues = new HashSet<>(Arrays.asList(listOfValues));
            return PropertyFactory.createProperty(propertyName, propertyType, propertyValue ,propertyDescription, fixedValues);
        } else {
            Property<?> ap = PropertyFactory.createProperty(propertyName, propertyType, propertyValue);
            ap.setDescription(propertyDescription);
            return ap;
        }
    }

}
