package org.ff4j.conf;

/*-
 * #%L
 * ff4j-core
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

import java.io.InputStream;
import java.text.SimpleDateFormat;

/**
 * Extension point to load configuration from multiple format files XML, YAML...
 * 
 * @author Cedrick LUNVEN (@clunven)
 *
 * @param <C>
 *      specialization of {@link FF4jConfiguration} is needed
 */
public interface FF4jConfigurationParser<C extends FF4jConfiguration> {
    
    String ENCODING = "UTF-8";
    
    String GLOBAL_AUDIT_TAG            = "audit";
    String GLOBAL_AUTOCREATE           = "autocreate";
    
    String FF4J_TAG                   = "ff4j";
    String FEATURES_TAG               = "features";
    String FEATURE_TAG                = "feature";
    String FEATURE_ATT_UID            = "uid";
    String FEATURE_ATT_DESC           = "description";
    String FEATURE_ATT_ENABLE         = "enable";
    String FEATURE_ATT_GROUP          = "groupName";
    String FEATURE_ATT_PERMISSIONS    = "permissions";
    String FEATURE_ATT_PROPERTIES     = "custom-properties";
    
    String TOGGLE_STRATEGY_TAG        = "flipstrategy";
    String TOGGLE_STRATEGY_ATTCLASS   = "class";
    String TOGGLE_STRATEGY_PARAMTAG   = "param";
    String TOGGLE_STRATEGY_PARAMNAME  = "name";
    String TOGGLE_STRATEGY_PARAMVALUE = "value";
    
    String PROPERTIES_TAG             = "properties";
    String PROPERTY_TAG               = "property";
    String PROPERTY_PARAMTYPE         = "type";
    String PROPERTY_PARAMNAME         = "name";
    String PROPERTY_PARAMDESCRIPTION  = "description";
    String PROPERTY_PARAMVALUE        = "value";
    String PROPERTY_PARAMFIXED_VALUES = "fixedValues";
    
    /** Date time format. */
    String DATE_FORMAT = "yyyy-MM-dd HH:mm:ss";
   
    /** expected expression. */
    SimpleDateFormat SIMPLE_DATE_FORMAT = new SimpleDateFormat(DATE_FORMAT);
    
    /**
     * Export configuration into binary file.
     * 
     * @param config
     *      configuration
     * @return
     *      stream to save as a file
     */
    InputStream export(C config);

    /**
     * Parsing of XML Configuration file.
     *
     * @param file
     *      target file
     * @return
     *      features and properties find within file
     */
    C parseConfigurationFile(InputStream in);
}
