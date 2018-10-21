package org.ff4j.parser;

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

import static org.ff4j.test.AssertUtils.assertHasLengthParam;
import static org.ff4j.test.AssertUtils.assertNotNullParam;

import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

import org.ff4j.parser.xml.XmlParserV2;

/**
 * Operations to be implemented to allow reading files in different format (XML or YAML)
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public abstract class AbstractConfigurationFileParser {
    
    /** 
     * <feature uid="first" enable="true" description="description"  groupName="GRP1">
     *  <acl>
     *   <permission name="FEATURE_TOGGLE" grantedUsers="pierre"   />
     *   <permission name="FEATURE_VIEW"   grantedRoles="EVERYONE" />
     *  </acl>
     *  <toggle-strategies>
     *   <toggle-strategy class="org.ff4j.strategy.PonderationStrategy">
     *    <param name="weight" value="0.5"/>
     *   </toggle-strategy>
     *   <toggle-strategy name="Ponderation">
     *    <param name="weight" value="0.5"/>
     *   </toggle-strategy>
     *  </toggle-strategies>
     *  <custom-properties>
     *   <property name="ppListInt" value="12,13,14" />
     *   <property name="digitValue" value="1" type="org.ff4j.property.domain.PropertyInt" fixedValues="0,1,2,3" />
     *  </custom-properties>
     * </feature>
     **/
    public static final String FF4J_TAG             = "ff4j";
    public static final String FEATURES_TAG         = "features";
    public static final String FEATURE_TAG          = "feature";
    public static final String FEATURE_ATT_UID      = "uid";
    public static final String FEATURE_ATT_DESC     = "description";
    public static final String FEATURE_ATT_ENABLE   = "enable";
    public static final String FEATURE_ATT_GROUP    = "groupName";
    public static final String FEATUREGROUP_TAG     = "feature-group";
    public static final String FEATUREGROUP_ATTNAME = "name";
    
    public static final String TOGGLE_STRATEGIES_TAG      = "toggle-strategies";
    public static final String TOGGLE_STRATEGY_TAG        = "toggle-strategy";
    public static final String TOGGLE_STRATEGY_ATTCLASS   = "class";
    public static final String TOGGLE_STRATEGY_PARAMTAG   = "param";
    public static final String TOGGLE_STRATEGY_PARAMNAME  = "name";
    public static final String TOGGLE_STRATEGY_PARAMVALUE = "value";
    public static final String PROPERTIES_CUSTOM_TAG      = "custom-properties";
    public static final String PROPERTIES_TAG             = "properties";
    
    /**
     * <properties>
     *  <property name="a" value="AMER" fixedValues="AMER,EAST" />
     * </properties>
     */
    public static final String PROPERTY_TAG               = "property";
    public static final String PROPERTY_PARAMTYPE         = "type";
    public static final String PROPERTY_PARAMNAME         = "name";
    public static final String PROPERTY_PARAMDESCRIPTION  = "description";
    public static final String PROPERTY_PARAMVALUE        = "value";
    public static final String PROPERTY_PARAMFIXED_VALUES = "fixedValues";
    public static final String PROPERTY_PARAM_READONLY    = "readOnly";
    
    /** 
     * <roles>
     *   <role name="aa" >
     *     <permission>BBB</permission>
     *     <permission>CCC</permission>
     *   </role>
     * </roles>
     */ 
    public static final String SECURITY_ROLES_TAG       = "roles";
    public static final String SECURITY_ROLE_TAG        = "role";
    public static final String SECURITY_ROLE_ATTNAME    = "name";
    public static final String SECURITY_PERMISSION_TAG  = "permission";
    public static final String SECURITY_PERMISSIONS_TAG = "permissions";
    
    /** 
     * <users>
     *  <user uid="11" firstName="22" lastName="33" description="44" >
     *   <roles>
     *    <role>USER</role>
     *   </roles>
     *   <permissions>
     *    <permission>ADMIN_FEATURES</permission>
     *    <permission>ADMIN_PROPERTIES</permission>
     *   </permissions>
     *  </user>
     * </users>
     */
    public static final String USERS_TAG                   = "users";
    public static final String USER_TAG                    = "user";
    public static final String USER_ATT_UID                = "uid";
    public static final String USER_ATT_DESC               = "description";
    public static final String USER_ATT_LASTNAME           = "lastName";
    public static final String USER_ATT_FIRSTNAME          = "firstName";
    public static final String USER_ATT_PERMISSIONS        = "permissions";
    public static final String USER_ATT_ROLES              = "roles";
    
    public static final String GLOBAL_PERMISSIONS_TAG      = "permissions";
    public static final String GLOBAL_PERMISSIONS_NAME     = "name";
    public static final String GLOBAL_PERMISSIONS_TARGET   = "target";
    public static final String GLOBAL_PERMISSIONS_USERS    = "users";
    public static final String GLOBAL_PERMISSIONS_ROLES    = "roles";
    public static final String GLOBAL_AUDIT_TAG            = "audit";
    public static final String GLOBAL_AUTOCREATE           = "autocreate";
    
    /** Do not parse the same file multiple times. */
    protected static Map < String, FF4jConfigFile > cachedConfiguration = new HashMap<>();
    
    /**
     * Parse file and marshall configuration for this object.
     *
     * @param fileName
     *      current filename.
     * @return
     *      configuration bean
     */
    public FF4jConfigFile parse(String fileName) {
        assertHasLengthParam("fileName", 0, fileName);
        if (!cachedConfiguration.containsKey(fileName)) {
            InputStream xmlIN = XmlParserV2.class.getClassLoader().getResourceAsStream(fileName);
            assertNotNullParam("fileName", 0, xmlIN, String.format("Cannot parse file : %s file not found", fileName));
            cachedConfiguration.put(fileName, parse(xmlIN));
        }
        return cachedConfiguration.get(fileName);
    }
    
    /**
     * Export current configuration in provided file
     *
     * @param fileName
     *      current filename.
     * @param targetFile
     *      file to write configuration
     * @return
     *      configuration bean
     */
    public void export(FF4jConfigFile config, String targetFile) {
        try (FileWriter out = new FileWriter(targetFile)) {
            out.write(export(config));
        } catch (IOException e) {
            throw new IllegalArgumentException("Cannot export data ", e);
        }
    }
    
    /**
     * Parse file and marshall configuration for this object.
     *
     * @param fileName
     *      current filename.
     * @return
     *      configuration bean
     */
    public abstract FF4jConfigFile parse(InputStream inputStream);
    
    /**
     * Export current configuration as inputStream.
     *
     * @param fileName
     *      current filename.
     * @return
     *      configuration bean
     */
    public abstract String export(FF4jConfigFile config);

}
