package org.ff4j.parser.yaml;

/*-
 * #%L
 * ff4j-config-yaml
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

import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.nio.charset.StandardCharsets;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.ff4j.conf.FF4jConfiguration;
import org.ff4j.conf.FF4jConfigurationParser;
import org.ff4j.core.Feature;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyString;
import org.ff4j.utils.MappingUtil;
import org.ff4j.utils.Util;
import org.yaml.snakeyaml.LoaderOptions;
import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.SafeConstructor;

/**
 * Parser to read {@link FF4jConfiguration} from a YAML file.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class YamlParser implements FF4jConfigurationParser<FF4jConfiguration> {
    
    /**
     * Initialization of the Yaml parser
     */
    private Yaml safeYaml = new Yaml(new SafeConstructor(new LoaderOptions()));
    
    /**
     * Default constructor.
     */
    public YamlParser() {}
    
    /** {@inheritDoc} */
    @Override
    public InputStream export(FF4jConfiguration ff4jConfig) {
        Util.assertNotNull(ff4jConfig);
        StringBuilder yamlFile =  new StringBuilder()
            .append(yamlKey(FF4J_TAG, 0, false))
            .append(yamlValue(GLOBAL_AUTOCREATE, ff4jConfig.isAutoCreate(), 2, false))
            .append(yamlValue(GLOBAL_AUDIT_TAG, ff4jConfig.isAudit(), 2, false));
        
        // Features
        if (null != ff4jConfig.getFeatures() && !ff4jConfig.getFeatures().isEmpty()) {
            yamlFile.append(yamlKey(FEATURES_TAG, 2, false));
            for (Feature f : ff4jConfig.getFeatures().values()) {
                yamlFile.append(yamlValue(FEATURE_ATT_UID, f.getUid(), 4, true));
                yamlFile.append(yamlValue(FEATURE_ATT_ENABLE, f.isEnable(), 6, false));
                if (null != f.getDescription()) {
                    yamlFile.append(yamlValue(FEATURE_ATT_DESC, f.getDescription(), 6, false));
                }
                if (null != f.getGroup()) {
                    yamlFile.append(yamlValue(FEATURE_ATT_GROUP, f.getGroup(), 6, false));
                }
                if (!f.getPermissions().isEmpty()) {
                    yamlFile.append(yamlValue(FEATURE_ATT_PERMISSIONS, f.getPermissions(), 6, false));
                }
                if (null != f.getFlippingStrategy()) {
                    yamlFile.append(yamlKey(TOGGLE_STRATEGY_TAG, 6, false));
                    yamlFile.append(yamlValue(TOGGLE_STRATEGY_ATTCLASS, f.getFlippingStrategy().getClass().getName(), 8, false));
                    yamlFile.append(yamlKey(TOGGLE_STRATEGY_PARAMTAG, 8, false));
                    for (Map.Entry<String, String> entry : f.getFlippingStrategy().getInitParams().entrySet()) {
                        yamlFile.append(yamlValue(TOGGLE_STRATEGY_PARAMNAME, entry.getKey(), 10, true));
                        yamlFile.append(yamlValue(TOGGLE_STRATEGY_PARAMVALUE, entry.getValue(), 12, false));
                    }
                }
                if (!f.getCustomProperties().isEmpty()) {
                    yamlFile.append(yamlKey(FEATURE_ATT_PROPERTIES, 6, false));
                    if (!f.getCustomProperties().isEmpty()) {
                        f.getCustomProperties().values().stream().forEach(p-> {
                            yamlFile.append(yamlProperty(p,8));
                        });
                    }
                }
            }
        }
        
        // Properties
        if (null != ff4jConfig.getProperties() && !ff4jConfig.getProperties().isEmpty()) {
            yamlFile.append(yamlKey(PROPERTIES_TAG, 2, false));
            for (Property<?> prop : ff4jConfig.getProperties().values()) {
                yamlFile.append(yamlProperty(prop, 4));
            }
        }
        String output = yamlFile.toString();
        System.out.println(output);
        return new ByteArrayInputStream(output.getBytes(StandardCharsets.UTF_8));
    }

    /** {@inheritDoc} */
    @Override
    @SuppressWarnings("unchecked")
    public FF4jConfiguration parseConfigurationFile(InputStream inputStream) {
        Util.assertNotNull(inputStream, "Cannot read file stream is empty, check readability and path.");
        // Strengthen serialization
        Map<?,?> yamlConfigFile = safeYaml.load(inputStream);
        Map<?,?> ff4jYamlMap = (Map<?, ?>) yamlConfigFile.get(FF4J_TAG);
        FF4jConfiguration ff4jConfig = new FF4jConfiguration();
        if (ff4jYamlMap != null) {
            // Audit
            if (ff4jYamlMap.containsKey(GLOBAL_AUDIT_TAG)) {
                ff4jConfig.setAudit(Boolean.valueOf(ff4jYamlMap.get(GLOBAL_AUDIT_TAG).toString()));
            }
            // AutoCreate
            if (ff4jYamlMap.containsKey(GLOBAL_AUTOCREATE)) {
                ff4jConfig.setAutoCreate(Boolean.valueOf(ff4jYamlMap.get(GLOBAL_AUTOCREATE).toString()));
            }
            // Properties
            ff4jConfig.getProperties()
                      .putAll(parseProperties((List<Map<String, Object>>) ff4jYamlMap.get(PROPERTIES_TAG))
            );
            // Features
            parseFeatures(ff4jConfig, (List<Map<String, Object>>) ff4jYamlMap.get(FEATURES_TAG));
        }
        return ff4jConfig;
    }
    
    @SuppressWarnings("unchecked")
    private Map < String, Property<?>> parseProperties(List<Map<String, Object>> properties) {
        Map < String, Property<?>> result = new HashMap<>();
        if (null != properties) {
            properties.forEach(property -> {
                // Initiate with name and value
                String name     = (String) property.get(PROPERTY_PARAMNAME);
                if (null == name) { 
                    throw new IllegalArgumentException("Invalid YAML File: 'name' is expected for properties");
                }
                
                Object objValue = property.get(PROPERTY_PARAMVALUE);
                if (null == objValue) {
                    throw new IllegalArgumentException("Invalid YAML File: 'value' is expected for properties");
                }
                // Convert as a String
                String strValue = String.valueOf(objValue);
                if (objValue instanceof Date) {
                    strValue = SIMPLE_DATE_FORMAT.format((Date) objValue);
                }
                
                Property<?> ap = new PropertyString(name, strValue);
                String optionalType = (String) property.get(PROPERTY_PARAMTYPE);
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
                String description = (String) property.get(PROPERTY_PARAMDESCRIPTION);
                if (null != description) {
                    ap.setDescription(description);
                } 
                // Fixed Values
                List<Object> fixedValues = (List<Object>) property.get(PROPERTY_PARAMFIXED_VALUES);
                if (null != fixedValues && fixedValues.size() > 0) {
                    fixedValues.stream().map(Object::toString).forEach(ap::add2FixedValueFromString);
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
            });
        }
        return result;
    }
    
    /**
     * Parsing YAML TAGS.
     *
     * @param ff4jConfig
     *      current configuration
     * @param features
     *      feature to populate
     */
    @SuppressWarnings("unchecked")
    private void parseFeatures(FF4jConfiguration ff4jConfig, List<Map<String, Object>> features) {
        if (null != features) {
            features.forEach(feature -> {
                String name = (String) feature.get(FEATURE_ATT_UID);
                if (null == name) throw new IllegalArgumentException("Invalid YAML File: 'uid' is expected for feature");
                Feature f = new Feature(name);
                // Enabled
                Boolean enabled = (Boolean) feature.get(FEATURE_ATT_ENABLE);
                if (null != enabled) f.setEnable(enabled);
                // Description
                String description = (String) feature.get(FEATURE_ATT_DESC);
                if (null != description) f.setDescription(description);
                // Group
                String groupName = (String) feature.get(FEATURE_ATT_GROUP);
                if (null != groupName) f.setGroup(groupName);
                // Permissions
                List<String> customPermissons = (List<String>) feature.get(FEATURE_ATT_PERMISSIONS);
                if (customPermissons != null) {
                    f.setPermissions(new HashSet<>(customPermissons));
                }
                // Toggle Strategies
                Map<String, Object> mapFlipStrategy = (Map<String, Object>) feature.get(TOGGLE_STRATEGY_TAG);
                if (null != mapFlipStrategy) {
                    f.setFlippingStrategy(parseFlipStrategy(f, mapFlipStrategy));
                }
                // Custom Properties
                List<Map<String, Object>> customProperties = (List<Map<String, Object>>) feature.get(FEATURE_ATT_PROPERTIES);
                if (customProperties != null) {
                    f.setCustomProperties(parseProperties(customProperties));
                }
                ff4jConfig.getFeatures().put(f.getUid(), f);
            });
        }
    }
    
    @SuppressWarnings("unchecked")
    private FlippingStrategy parseFlipStrategy(Feature feature, Map<String, Object> toggleStrategy) {
        try {
            // Parse class
            String clazzName = (String) toggleStrategy.get(TOGGLE_STRATEGY_ATTCLASS);
            Class<?> typeClass = Class.forName(clazzName);
            if (!FlippingStrategy.class.isAssignableFrom(typeClass)) {
                throw new IllegalArgumentException("Cannot create flipstrategy <" + clazzName + "> invalid type");
            }
            FlippingStrategy flipStrategy = (FlippingStrategy) typeClass.newInstance();
            // Parse Params
            List<Map<String, Object>> mapYamlParam = (List<Map<String, Object>>) toggleStrategy.get(TOGGLE_STRATEGY_PARAMTAG);
            Map<String,String> params = new HashMap<>();
            for (Map<String, Object> currentParam : mapYamlParam) {
                params.put(
                        currentParam.get(TOGGLE_STRATEGY_PARAMNAME).toString(), 
                        currentParam.get(TOGGLE_STRATEGY_PARAMVALUE).toString());
            }
            flipStrategy.init(feature.getUid(), params);
            return flipStrategy;
        } catch (Exception e) {
            throw new IllegalArgumentException("An error occurs during flipstrategy parsing TAG" + feature.getUid(), e);
        }
    }
    
    /**
     * Create a {@link Property} in the YAML.
     *
     * @param <T>
     *      property generic
     * @param p
     *      property
     * @param offset
     *      current offset to create the property bloc
     * @return
     */
    private static final <T> String yamlProperty(Property<T> p, int offset) {
        StringBuilder yamlProp = new StringBuilder();
        yamlProp.append(yamlValue(PROPERTY_PARAMNAME, p.getName(), offset, true));
        yamlProp.append(yamlValue(PROPERTY_PARAMTYPE, p.getClass().getCanonicalName(), offset+2, false));
        yamlProp.append(yamlValue(PROPERTY_PARAMVALUE, p.asString(), offset+2, false));
        if (null != p.getFixedValues() && !p.getFixedValues().isEmpty()) {
            yamlProp.append(yamlValue(PROPERTY_PARAMFIXED_VALUES, p.getFixedValues(), offset+2, false));
        }
        return yamlProp.toString();
    }
    
    /**
     * Write a YAML key key:
     * 
     * @param key
     *      yaml key
     * @param value
     *      yaml value
     * @param offset
     *      offset in the yaml
     * @param isList
     *      if value is a list
     * @return
     *      string to be added to YAML
     */
    private static final String yamlKey(String key, int offset, boolean isList) {
        return  new StringBuilder()
                    .append(new String(new char[offset]).replace('\0', ' '))
                    .append(isList ? "- " : "")
                    .append(key).append(": ")
                    .append("\n").toString();
    }
    
    /**
     * Write a YAML value  key:value
     * 
     * @param key
     *      yaml key
     * @param value
     *      yaml value
     * @param offset
     *      offset in the yaml
     * @param isList
     *      if value is a list
     * @return
     *      string to be added to YAML
     */
    private static final String yamlValue(String key, Object value, int offset, boolean isList) {
       return new StringBuilder()
                   .append(new String(new char[offset]).replace('\0', ' '))
                   .append(isList ? "- " : "")
                   .append(key).append(": ")
                   .append(value)
                   .append("\n").toString();
    }
    
}
