package org.ff4j.parser.properties;

/*-
 * #%L
 * ff4j-config-properties
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import java.io.ByteArrayInputStream;
import java.io.IOException;

import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.stream.Collectors;

import org.ff4j.conf.FF4jConfiguration;
import org.ff4j.conf.FF4jConfigurationParser;
import org.ff4j.core.Feature;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyString;
import org.ff4j.utils.MappingUtil;
import org.ff4j.utils.Util;

/**
 * Parser to read {@link FF4jConfiguration} from a YAML file.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertiesParser implements FF4jConfigurationParser<FF4jConfiguration> {
    
    /**
     * Default constructor.
     */
    public PropertiesParser() {}
    
    /**
     * Syntax sugar to append a key to file
     */
    private void addProperty(StringBuilder file, String key, Object value) {
        if (null == value) value = "";
        file.append(key + "=" + String.valueOf(value) + "\n");
    }
    
    /**
     * Utility to serialize a property
     */
    private void exportProperty(StringBuilder output, String prefix, Property<?> p) {
        addProperty(output, prefix + PROPERTY_PARAMNAME,  p.getName());
        addProperty(output, prefix + PROPERTY_PARAMTYPE,  p.getClass().getCanonicalName());
        addProperty(output, prefix + PROPERTY_PARAMVALUE,  p.asString());
        if (null != p.getFixedValues() && !p.getFixedValues().isEmpty()) {
            addProperty(output, prefix + PROPERTY_PARAMFIXED_VALUES, 
                    String.join(",", p.getFixedValues().stream().map(Object::toString).collect(Collectors.toSet())));
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public InputStream export(FF4jConfiguration ff4jConfig) {
        Util.assertNotNull(ff4jConfig);
        StringBuilder output =  new StringBuilder();
        addProperty(output, FF4J_TAG + "." + GLOBAL_AUDIT_TAG, ff4jConfig.isAudit());
        addProperty(output, FF4J_TAG + "." + GLOBAL_AUTOCREATE, ff4jConfig.isAutoCreate());
            
        // Features
        if (null != ff4jConfig.getFeatures()) {
            int idxFeature = 0;
            for (Feature f : ff4jConfig.getFeatures().values()) {
                // ff4j.features.x.
                String prefixKey = FF4J_TAG + "." + FEATURES_TAG + "." + idxFeature + ".";
                addProperty(output, prefixKey + FEATURE_ATT_UID, f.getUid());
                addProperty(output, prefixKey + FEATURE_ATT_ENABLE, f.isEnable());
                if (null != f.getDescription()) {
                    addProperty(output, prefixKey + FEATURE_ATT_DESC, f.getDescription());
                }
                if (null != f.getGroup()) {
                    addProperty(output, prefixKey + FEATURE_ATT_GROUP, f.getGroup());
                }
                if (!f.getPermissions().isEmpty()) {
                    addProperty(output, prefixKey + FEATURE_ATT_PERMISSIONS, String.join(",", f.getPermissions()));
                }
                if (null != f.getFlippingStrategy()) {
                    String flipKey = prefixKey + TOGGLE_STRATEGY_TAG + ".";
                    addProperty(output, flipKey + TOGGLE_STRATEGY_ATTCLASS, 
                            String.join(",", f.getFlippingStrategy().getClass().getName()));
                    int idxParam = 0;
                    for (Map.Entry<String, String> entry : f.getFlippingStrategy().getInitParams().entrySet()) {
                        addProperty(output, flipKey + TOGGLE_STRATEGY_PARAMTAG + "." + idxParam + "." + TOGGLE_STRATEGY_PARAMNAME, entry.getKey());
                        addProperty(output, flipKey + TOGGLE_STRATEGY_PARAMTAG + "." + idxParam + "." + TOGGLE_STRATEGY_PARAMVALUE, entry.getValue());
                        idxParam++;
                    }
                }
                if (!f.getCustomProperties().isEmpty()) {
                    int idxProps = 0;
                    for (Property<?> p : f.getCustomProperties().values()) {
                        String propKey = prefixKey + FEATURE_ATT_PROPERTIES + "." + idxProps + ".";
                        exportProperty(output, propKey, p);
                        idxProps++;
                    }
                }
                idxFeature++;
            }
        }
        
        // Properties
        if (null != ff4jConfig.getProperties() && !ff4jConfig.getProperties().isEmpty()) {
            int idxProps = 0;
            for (Property<?> p : ff4jConfig.getProperties().values()) {
                String propKey = FF4J_TAG + "." + PROPERTIES_TAG + "." + idxProps + ".";
                exportProperty(output, propKey, p);
                idxProps++;
            }
        }
        System.out.println(output.toString());
        return new ByteArrayInputStream(output.toString().getBytes(StandardCharsets.UTF_8));
    }
    
    /**
     * Load properties from environments.
     */
    public FF4jConfiguration parseSystemConfiguration() {
        return parseConfiguration(System.getProperties());
    }
    
    /** {@inheritDoc} */
    @Override
    public FF4jConfiguration parseConfigurationFile(InputStream inputStream) {
        Util.assertNotNull(inputStream, "Cannot read file stream is empty, check readability and path.");
        try {
            Properties props = new Properties();
            props.load(inputStream);
            return parseConfiguration(props);
        } catch (IOException e) {
            throw new IllegalArgumentException("Cannot read property files");
        }
    }
    
    /**
     * Load properties from environments.
     */
    public FF4jConfiguration parseConfiguration(Properties props) {
        Util.assertNotNull(props, "Cannot parse null properties");
        Map<String, String> mapProperties = new HashMap<String, String>();
        for (final String name: props.stringPropertyNames()) {
                mapProperties.put(name, props.getProperty(name));
        }
        return parseConfiguration(mapProperties);
    }
    
    /**
     * Load properties from environments.
     */
    public FF4jConfiguration parseConfiguration(Map<String, String> mapProperties) {
        Util.assertNotNull(mapProperties, "Cannot parse null properties");
        FF4jConfiguration ff4jConfig = new FF4jConfiguration();
        // Audit
        if (mapProperties.containsKey(FF4J_TAG + "." + GLOBAL_AUDIT_TAG)) {
            ff4jConfig.setAudit(Boolean.valueOf(mapProperties.get(FF4J_TAG + "." + GLOBAL_AUDIT_TAG)));
        }
        // AutoCreate
        if (mapProperties.containsKey(FF4J_TAG + "." + GLOBAL_AUTOCREATE)) {
            ff4jConfig.setAutoCreate(Boolean.valueOf(mapProperties.get(FF4J_TAG + "." + GLOBAL_AUTOCREATE)));
        }
        // Properties
        ff4jConfig.getProperties().putAll(parseProperties(FF4J_TAG + "." + PROPERTIES_TAG, mapProperties));
        // Features
        parseFeatures(ff4jConfig, mapProperties);
        return ff4jConfig;
    } 
    
    private void assertKeyNotEmpty(Map<String, String> mapConfigProperties , String keyName) {
        String strValue = mapConfigProperties.get(keyName);
        if (null == strValue || "".equals(strValue)) {
            throw new IllegalArgumentException("Key [" + keyName + "] is required and value cannot be empty");
        }
    }
    
    /**
     * Parse properties:
     *
     * ff4j.properties.0.name=...
     * ff4j.properties.0.type=...
     * ff4j.properties.0.description=...
     * ff4j.properties.0.value=...
     * ff4j.properties.0.fixedValues=...
     * 
     * ff4j.features.0.custom-properties.0.name=...
     * ff4j.features.0.custom-properties.0.type=...
     * ff4j.features.0.custom-properties.0.description=...
     * ff4j.features.0.custom-properties.0.value=...
     * ff4j.features.0.custom-properties.0.fixedValues=...
     */
    private Map < String, Property<?>> parseProperties(String prefix, Map<String, String> mapConfigProperties) {
        Map < String, Property<?>> result = new HashMap<>();
        int idx = 0;
        String currentPropertyKey = prefix  + "." + idx;
        while (mapConfigProperties.containsKey(currentPropertyKey +  "." + PROPERTY_PARAMNAME)) {
            
            assertKeyNotEmpty(mapConfigProperties, currentPropertyKey +  "." + PROPERTY_PARAMNAME);
            String name     = mapConfigProperties.get(currentPropertyKey +  "." + PROPERTY_PARAMNAME);
            
            assertKeyNotEmpty(mapConfigProperties, currentPropertyKey +  "." + PROPERTY_PARAMVALUE);
            String strValue = mapConfigProperties.get(currentPropertyKey +  "." + PROPERTY_PARAMVALUE);
            
            Property<?> ap      = new PropertyString(name, strValue);
            String optionalType = mapConfigProperties.get(currentPropertyKey + "." + PROPERTY_PARAMTYPE);
            // If specific type defined ?
            if (null != optionalType) {
                // Substitution if relevant (e.g. 'int' -> 'org.ff4j.property.PropertyInt')
                optionalType = MappingUtil.mapPropertyType(optionalType);
                try {
                    // Constructor (String, String) is mandatory in Property interface
                    Class<?> typeClass = Class.forName(optionalType);
                    if (!Property.class.isAssignableFrom(typeClass)) {
                        throw new IllegalArgumentException("Cannot create property <" + name + "> invalid type <" + optionalType + ">");
                    }
                    Constructor<?> constr = typeClass.getConstructor(String.class, String.class);
                    ap = (Property<?>) constr.newInstance(name, strValue);
                } catch (Exception e) {
                    throw new IllegalArgumentException("Cannot instantiate '" + optionalType + "' check default constructor", e);
                }
            }
            // Description
            String description = mapConfigProperties.get(currentPropertyKey + "." + PROPERTY_PARAMDESCRIPTION);
            if (null != description) {
                ap.setDescription(description);
            } 
            // Fixed Values
            String strFixedValues = mapConfigProperties.get(currentPropertyKey + "." + PROPERTY_PARAMFIXED_VALUES);
            if (null != strFixedValues && !"".equals(strFixedValues)) {
                Arrays.asList(strValue.split(","))
                      .stream()
                      .map(String::trim)
                      .forEach(ap::add2FixedValueFromString);
            }
          
            // Check fixed value
            if (ap.getFixedValues() != null &&  
               !ap.getFixedValues().isEmpty() && 
               !ap.getFixedValues().contains(ap.getValue())) {
                throw new IllegalArgumentException("Cannot create property <" + ap.getName() + 
                        "> invalid value <" + ap.getValue() + 
                        "> expected one of " + ap.getFixedValues());
            }
            result.put(ap.getName(), ap);
            // ff4j.properties.X
            currentPropertyKey = prefix + "." + ++idx;
        }
        return result;
    }
    
    /**
     * Parse Features.
     *
     * @param ff4jConfig
     *      configuration object to populate
     * @param mapProperties
     *      properties
     */
    private void parseFeatures(FF4jConfiguration ff4jConfig, Map<String, String> mapConf) {
        int idx = 0;
        String currentFeatureKey = FF4J_TAG + "." + FEATURES_TAG  + "." + idx;
        while (mapConf.containsKey(currentFeatureKey +  "." + FEATURE_ATT_UID)) {
            assertKeyNotEmpty(mapConf, currentFeatureKey +  "." + FEATURE_ATT_UID);
            Feature f = new Feature(mapConf.get(currentFeatureKey +  "." + FEATURE_ATT_UID));
            // Enabled
            assertKeyNotEmpty(mapConf, currentFeatureKey +  "." + FEATURE_ATT_ENABLE);
            f.setEnable(Boolean.valueOf(mapConf.get(currentFeatureKey +  "." + FEATURE_ATT_ENABLE)));
            // Description
            String description = mapConf.get(currentFeatureKey +  "." + FEATURE_ATT_DESC);
            if (null != description && !"".equals(description)) {
                f.setDescription(description);
            }
            // Group
            String groupName = mapConf.get(currentFeatureKey +  "." + FEATURE_ATT_GROUP);
            if (null != groupName && !"".equals(groupName)) {
                f.setGroup(groupName);
            }
            // Permissions
            String strPermissions = mapConf.get(currentFeatureKey +  "." + FEATURE_ATT_PERMISSIONS);
            if (null != strPermissions && !"".equals(strPermissions)) {
                f.setPermissions(
                        Arrays.asList(strPermissions.split(","))
                              .stream()
                              .map(String::trim)
                              .collect(Collectors.toSet()));
            }
            // Custom Properties
            f.setCustomProperties(parseProperties(currentFeatureKey + "." + FEATURE_ATT_PROPERTIES, mapConf));
            // FlipStrategy
            String flipStrategyClass = mapConf.get(currentFeatureKey +  "." + TOGGLE_STRATEGY_TAG + "." + TOGGLE_STRATEGY_ATTCLASS);
            if (null != flipStrategyClass && !"".equals(flipStrategyClass)) {
                FlippingStrategy flipStrategy = null;
                try {
                    Class<?> typeClass = Class.forName(flipStrategyClass);
                    if (!FlippingStrategy.class.isAssignableFrom(typeClass)) {
                        throw new IllegalArgumentException("Cannot create flipstrategy <" + flipStrategyClass + "> invalid type");
                    }
                    flipStrategy = (FlippingStrategy) typeClass.newInstance();
                } catch (Exception e) {
                    throw new IllegalArgumentException("Cannot parse flipStrategy for feature '" + f.getUid() + 
                            "' -> check key [" + currentFeatureKey +  "." + TOGGLE_STRATEGY_TAG + "." + TOGGLE_STRATEGY_ATTCLASS + "]", e);
                }
                int idxParam = 0;
                String currentParamKey = currentFeatureKey +  "." + TOGGLE_STRATEGY_TAG + "." + TOGGLE_STRATEGY_PARAMTAG + "." + idxParam;
                Map<String, String> params = new HashMap<>();
                while (mapConf.containsKey(currentParamKey+  "." + TOGGLE_STRATEGY_PARAMNAME)) {
                    assertKeyNotEmpty(mapConf, currentParamKey + "." + TOGGLE_STRATEGY_PARAMNAME);
                    assertKeyNotEmpty(mapConf, currentParamKey + "." + TOGGLE_STRATEGY_PARAMVALUE);
                    params.put(mapConf.get(currentParamKey + "." + TOGGLE_STRATEGY_PARAMNAME), 
                               mapConf.get(currentParamKey + "." + TOGGLE_STRATEGY_PARAMVALUE));
                    currentParamKey = currentFeatureKey +  "." + TOGGLE_STRATEGY_TAG + "." + TOGGLE_STRATEGY_PARAMTAG + "." + ++idxParam;
                }
                flipStrategy.init(f.getUid(), params);
                f.setFlippingStrategy(flipStrategy);
                
            }
            ff4jConfig.getFeatures().put(f.getUid(), f);
            
            // ff4j.features.X
            currentFeatureKey = FF4J_TAG + "." + FEATURES_TAG  + "." + ++idx;
        }
        
    }
    
    
    
}
