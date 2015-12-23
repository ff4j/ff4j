package org.ff4j.neo4j.mapper;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

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
import org.ff4j.neo4j.FF4jNeo4jConstants;
import org.ff4j.neo4j.FF4jNeo4jLabels;
import org.ff4j.property.AbstractProperty;
import org.ff4j.property.Property;
import org.ff4j.utils.json.FeatureJsonParser;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;

/**
 * Map Neo4j node from and to {@link Feature} bean.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FeatureNeo4jMapper implements FF4jNeo4jConstants {
    
    /**
     * Hide default constructor.
     */
    private FeatureNeo4jMapper() {
    }
    
    /**
     * Transform  node FF4J_FEATURE into core Feature.
     *
     * @param nodeFeature
     * @return
     */
    public static Feature fromNode2Feature(Node nodeFeature) {
        String featureID    = (String) nodeFeature.getProperty(NODEFEATURE_ATT_UID);
        Boolean enable      = (Boolean) nodeFeature.getProperty(NODEFEATURE_ATT_ENABLE);
        String description  = (String) nodeFeature.getProperty(NODEFEATURE_ATT_DESCRIPTION);
        String[] roles      = (String[]) nodeFeature.getProperty(NODEFEATURE_ATT_ROLES);
        Feature feature = new Feature(featureID, enable);
        if (description != null) {
            feature.setDescription(description);
        }
        if (roles != null) {
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
        return FeatureJsonParser.parseFlipStrategy(featureUid, className, initParams);       
    }
    
    
    /**
     * Mapping from Neo4j node to {@link AbstractProperty}
     * 
     * @param featureUid
     *      unique feature identifier
     * @param nodeProperty
     *      node property
     * @return
     *      value of node
     */
    static public AbstractProperty<?> fromNode2Property(String featureUid, Node nodeProperty) {
        Map < String, Object > nodeProperties = nodeProperty.getAllProperties();
        
        if (!nodeProperties.containsKey(NODEPROPERTY_ATT_NAME)) {
            throw new IllegalArgumentException(NODEPROPERTY_ATT_NAME + " is a required property");
        }
        
        if (!nodeProperties.containsKey(NODEPROPERTY_ATT_VALUE)) {
            throw new IllegalArgumentException(NODEPROPERTY_ATT_VALUE + " is a required property");
        }
        
        String propertyName  = (String) nodeProperty.getProperty(NODEPROPERTY_ATT_NAME);
        String propertyValue = (String) nodeProperty.getProperty(NODEPROPERTY_ATT_VALUE);
        AbstractProperty<?> ap = new Property(propertyName, propertyValue);
        
        // If specific type defined ?
        if (nodeProperties.containsKey(NODEPROPERTY_ATT_TYPE)) {
            
            String optionalType = (String) nodeProperty.getProperty(NODEPROPERTY_ATT_TYPE);

            try {
                // Construction by dedicated constructor with introspection
                Constructor<?> constr = Class.forName(optionalType).getConstructor(String.class, String.class);
                ap = (AbstractProperty<?>) constr.newInstance(propertyName, propertyValue);
            } catch (InstantiationException e) {
                throw new IllegalArgumentException("Cannot instantiate '" + optionalType + "' check default constructor", e);
            } catch (IllegalAccessException e) {
                throw new IllegalArgumentException("Cannot instantiate '" + optionalType + "' check visibility", e);
            } catch (ClassNotFoundException e) {
                throw new IllegalArgumentException("Cannot instantiate '" + optionalType + "' not found", e);
            } catch (InvocationTargetException e) {
                throw new IllegalArgumentException("Cannot instantiate '" + optionalType + "'  error within constructor", e);
            } catch (NoSuchMethodException e) {
                throw new IllegalArgumentException("Cannot instantiate '" + optionalType + "' constructor not found", e);
            } catch (SecurityException e) {
                throw new IllegalArgumentException("Cannot instantiate '" + optionalType + "' check constructor visibility", e);
            }
        }
        
        if (nodeProperties.containsKey(NODEPROPERTY_ATT_DESCRIPTION)) {
            ap.setDescription((String) nodeProperty.getProperty(NODEPROPERTY_ATT_DESCRIPTION));
        }
        
        // Is there any fixed Value
        if (nodeProperties.containsKey(NODEPROPERTY_ATT_FIXEDVALUES)) {
            String[] listOfValues = (String[]) nodeProperty.getProperty(NODEPROPERTY_ATT_FIXEDVALUES);
            if (listOfValues != null && listOfValues.length >0) {
                for (int l = 0; l < listOfValues.length;l++) {
                    ap.add2FixedValueFromString(listOfValues[l]);
                }
            }
        }
        
        // Check fixed value
        if (ap.getFixedValues() != null && !ap.getFixedValues().contains(ap.getValue())) {
            throw new IllegalArgumentException("Cannot create property <" + ap.getName() + 
                    "> invalid value <" + ap.getValue() + 
                    "> expected one of " + ap.getFixedValues());
        }
        
        return ap;
    }
    
    /**
     * Create Node from Feature.
     *
     * @param feature
     *      target feature
     * @return
     *      target node
     */
    static public Node fromFeature2Node(GraphDatabaseService graphDb, Feature feature) {
        Node nodeFeature = graphDb.createNode(FF4jNeo4jLabels.FF4J_FEATURE);
        nodeFeature.setProperty(NODEFEATURE_ATT_UID, feature.getUid());
        nodeFeature.setProperty(NODEFEATURE_ATT_ENABLE, feature.isEnable());
        nodeFeature.setProperty(NODEFEATURE_ATT_DESCRIPTION, feature.getDescription());
        if (feature.getPermissions() != null) {
            nodeFeature.setProperty(NODEFEATURE_ATT_ROLES, feature.getPermissions().toArray(new String[0]));
        }
        return nodeFeature;
    }
    
    

}
