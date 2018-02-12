package org.ff4j.parser.xml;

import static org.ff4j.test.AssertUtils.assertHasLengthParam;
import static org.ff4j.test.AssertUtils.assertNotNullParam;

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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.ff4j.feature.Feature;
import org.ff4j.feature.ToggleStrategy;
import org.ff4j.parser.FF4jConfigFile;
import org.ff4j.property.Property;
import org.ff4j.property.domain.PropertyString;
import org.ff4j.security.domain.FF4jPermission;
import org.ff4j.security.domain.FF4jUser;
import org.ff4j.test.AssertUtils;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Allow to parse XML files to load {@link Feature}.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public final class XmlParser {

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
    public static final String FEATURES_TAG         = "features";
    public static final String FEATURE_TAG          = "feature";
    public static final String FEATURE_ATT_UID      = "uid";
    public static final String FEATURE_ATT_DESC     = "description";
    public static final String FEATURE_ATT_ENABLE   = "enable";
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
    public static final String USERS_TAG                = "users";
    public static final String USER_TAG                 = "user";
    public static final String USER_ATT_UID             = "uid";
    public static final String USER_ATT_DESC            = "description";
    public static final String USER_ATT_LASTNAME        = "lastName";
    public static final String USER_ATT_FIRSTNAME       = "firstName";
    
    /** TAG XML. */
    public static final String CDATA_START = "<![CDATA[";

    /** TAG XML. */
    public static final String CDATA_END = "]]>";

    /** XML Generation constants. */
    private static final String ENCODING = "UTF-8";

    /** XML Generation constants. */
    private static final String XML_HEADER = 
            "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"//
            + "<ff4j xmlns=\"http://www.ff4j.org/schema/ff4j\""//
            + "\n xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\""//
            + "\n xsi:schemaLocation=\"http://www.ff4j.org/schema/ff4j http://ff4j.org/schema/ff4j-2.0.xsd\">"
            + ">\n\n";

    /** XML Generation constants. */
    private static final String XML_FEATURE = "  <feature uid=\"{0}\" enable=\"{1}\" ";

    /** XML Generation constants. */
    private static final String END_FEATURE = "  </feature>\n\n";

    /** XML Generation constants. */
    private static final String BEGIN_FEATURES = " <features>\n\n";
    
    /** XML Generation constants. */
    private static final String END_FEATURES = " </features>\n\n";
    
    /** XML Generation constants. */
    private static final String BEGIN_PROPERTIES = " <properties>\n\n";
    
    /** XML Generation constants. */
    private static final String BEGIN_CUSTOMPROPERTIES = "   <custom-properties>\n";
    
    /** XML Generation constants. */
    private static final String END_CUSTOMPROPERTIES = "   </custom-properties>\n";
    
    /** XML Generation constants. */
    private static final String END_PROPERTIES = " </properties>\n\n";
    
    /** XML Generation constants. */
    private static final String END_FF4J = "</ff4j>\n\n";
    
    /** Error message. */
    public static final String ERROR_SYNTAX_IN_CONFIGURATION_FILE = "Error syntax in configuration file : ";

    /** Document Builder use to parse XML. */
    private static DocumentBuilder builder = null;
   
    /** Do not parse the same file multiple times. */
    private static Map < String, FF4jConfigFile > cachedXmlData = new HashMap<>();
    
    /**  Hide constructor. */
    private XmlParser() {
    }
    
    /**
     * Parse configuration file.
     *
     * @param fileName
     *      target file
     * @return
     *      current configuration as XML
     */
    public static FF4jConfigFile parseFile(String fileName) {
        assertHasLengthParam("fileName", 0, fileName);
        if (!cachedXmlData.containsKey(fileName)) {
            InputStream xmlIN = XmlParser.class.getClassLoader().getResourceAsStream(fileName);
            assertNotNullParam("fileName", 0, xmlIN, String.format("Cannot parse XML file %s file not found", fileName));
            cachedXmlData.put(fileName, parseInputStream(xmlIN));
        }
        return cachedXmlData.get(fileName);
    }
    
    /**
     * Parsing of XML Configuration file.
     *
     * @param file
     *      target file
     * @return
     *      features and properties find within file
     */
    public static FF4jConfigFile parseInputStream(InputStream in) {
        try {
            FF4jConfigFile xmlConf = new FF4jConfigFile();
            NodeList firstLevelNodes = getDocumentBuilder()
                    .parse(in)
                    .getElementsByTagName("ff4j").item(0)
                    .getChildNodes();
            for (int i = 0; i < firstLevelNodes.getLength(); i++) {
                if (firstLevelNodes.item(i) instanceof Element) {
                    Element currentCore = (Element) firstLevelNodes.item(i);
                    
                    if (SECURITY_ROLES_TAG.equals(currentCore.getNodeName())) {
                        xmlConf.setRoles(parseRolesTag(currentCore));
                        
                    } else if (USERS_TAG.equals(currentCore.getNodeName())) {
                        xmlConf.setUsers(parseUsersTag(currentCore));
                        
                    } else if (FEATURES_TAG.equals(currentCore.getNodeName())) {
                        xmlConf.setFeatures(parseFeaturesTag(currentCore));
                        
                    } else if (PROPERTIES_TAG.equals(currentCore.getNodeName())) {
                        xmlConf.setProperties(parsePropertiesTag(currentCore));
                    }
                }
            }
            return xmlConf;
            
        } catch (Exception e) {
            throw new IllegalArgumentException("Cannot parse XML data, please check file access ", e);
        }
    }
    
    /**
     * Load map of {@link Feature} from an inpustream (containing xml text).
     * 
     * @param in
     *            inpustream with XML text
     * @return the sorted map of features
     * @throws IOException
     *             exception raised when reading inputstream
     */
    private static Map<String, FF4jUser> parseUsersTag(Element usersTag) {
        Map <String, FF4jUser > mapOfUsers = new LinkedHashMap<>();
        NodeList firstLevelNodes = usersTag.getChildNodes();
        for (int i = 0; i < firstLevelNodes.getLength(); i++) {
            if (firstLevelNodes.item(i) instanceof Element) {
                Element currentCore = (Element) firstLevelNodes.item(i);
                if (USER_TAG.equals(currentCore.getNodeName())) {
                    FF4jUser singleUser = parseUserTag(currentCore);
                    mapOfUsers.put(singleUser.getUid(), singleUser);
                } else {
                    throw new IllegalArgumentException("Invalid XML Format, Features sub nodes are [user]");
                }
            }
        }
        return mapOfUsers;
    }
    
    private static Optional < String > getOptionalAttribute(NamedNodeMap nnm, String attName) {
        return (null != nnm.getNamedItem(attName)) ?
             Optional.of(nnm.getNamedItem(attName).getNodeValue()) : Optional.empty();
    }
    
    private static String getRequiredAttribute(NamedNodeMap nnm, String attName) {
        AssertUtils.assertNotNull(nnm.getNamedItem(attName));
        return nnm.getNamedItem(attName).getNodeValue();
    }
    
    private static FF4jUser parseUserTag(Element userXmlTag) {
        NamedNodeMap nnm = userXmlTag.getAttributes();
        
        // Create user
        FF4jUser f = new FF4jUser(getRequiredAttribute(nnm, USER_ATT_UID));
        getOptionalAttribute(nnm, USER_ATT_FIRSTNAME).ifPresent(f::setFirstName);
        getOptionalAttribute(nnm, USER_ATT_LASTNAME).ifPresent(f::setLastName);
        getOptionalAttribute(nnm, USER_ATT_DESC).ifPresent(f::setDescription);
        
        NodeList firstLevelNodes = userXmlTag.getChildNodes();
        for (int i = 0; i < firstLevelNodes.getLength(); i++) {
            if (firstLevelNodes.item(i) instanceof Element) {
                Element currentCore = (Element) firstLevelNodes.item(i);
                // Roles
                if (SECURITY_ROLES_TAG.equals(currentCore.getNodeName())) {
                    NodeList roleNodes = currentCore.getChildNodes();
                    for (int j = 0; j < roleNodes.getLength(); j++) {
                        if (roleNodes.item(j) instanceof Element) {
                            f.getRoles().add(
                                    ((Element) roleNodes.item(j))
                                    .getTextContent().trim());
                        }
                    }
                    System.out.println("Roles " + f.getRoles());
                }
                // Permissions
                if (SECURITY_PERMISSIONS_TAG.equals(currentCore.getNodeName())) {
                    NodeList permNodes = currentCore.getChildNodes();
                    for (int j = 0; j < permNodes.getLength(); j++) {
                        if (permNodes.item(j) instanceof Element) {
                            String perStr =  ((Element) permNodes.item(j))
                                    .getTextContent().trim();
                            f.getPermissions().add(
                                    FF4jPermission.valueOf(perStr));
                        }
                    }
                    System.out.println("Permissions" + f.getPermissions());
                }
            }
        }
        return f;
    }
    
    /**
     * Load map of {@link Feature} from an inpustream (containing xml text).
     * 
     * @param in
     *            inpustream with XML text
     * @return the sorted map of features
     * @throws IOException
     *             exception raised when reading inputstream
     */
    private static Map<String, Set < String > > parseRolesTag(Element rolesTag) {
        Map<String, Set < String > > mapOfRoles = new HashMap<>();
        NodeList roleNodes = rolesTag.getChildNodes();
        for (int i = 0; i < roleNodes.getLength(); i++) {
            if (roleNodes.item(i) instanceof Element) {
                parseRoleTag((Element) roleNodes.item(i), mapOfRoles);
            }
        }
        return mapOfRoles;
    }
    
    private static void parseRoleTag(Element roleNode, Map<String, Set < String > > mapOfRoles) {
        NamedNodeMap nnm = roleNode.getAttributes();
        Node attributeName = nnm.getNamedItem(SECURITY_ROLE_ATTNAME);
        AssertUtils.assertNotNull(attributeName);
        mapOfRoles.put(attributeName.getNodeValue(), parseRolePermissionsTag(roleNode));
    }
    
    private static Set < String > parseRolePermissionsTag(Element roleNode) {
        Set<String> setOfPermissions = new HashSet<>();
        NodeList permissionNodes = roleNode.getChildNodes();
        for (int j = 0; j < permissionNodes.getLength(); j++) {
            if (permissionNodes.item(j) instanceof Element) {
                Element permissionNode = (Element) permissionNodes.item(j);
                setOfPermissions.add(permissionNode.getTextContent().trim());
            }
        }
        return setOfPermissions;
    }
    
    /**
     * Load map of {@link Feature} from an inpustream (containing xml text).
     * 
     * @param in
     *            inpustream with XML text
     * @return the sorted map of features
     * @throws IOException
     *             exception raised when reading inputstream
     */
    private static Map<String, Feature> parseFeaturesTag(Element featuresTag) {
        Map<String, Feature> xmlFeatures = new LinkedHashMap<String, Feature>();
        NodeList firstLevelNodes = featuresTag.getChildNodes();
        for (int i = 0; i < firstLevelNodes.getLength(); i++) {
            if (firstLevelNodes.item(i) instanceof Element) {
                Element currentCore = (Element) firstLevelNodes.item(i);
                if (FEATURE_TAG.equals(currentCore.getNodeName())) {
                    Feature singleFeature = parseFeatureTag(currentCore);
                    xmlFeatures.put(singleFeature.getUid(), singleFeature);
                } else if (FEATUREGROUP_TAG.equals(currentCore.getNodeName())) {
                    xmlFeatures.putAll(parseFeatureGroupTag(currentCore));
                } else {
                    throw new IllegalArgumentException("Invalid XML Format, Features sub nodes are [feature,feature-group]");
                }
            }
        }
        return xmlFeatures;
    }

    /**
     * Parse TAG &lt;feature-group&gt;.
     * 
     * @param featGroupTag
     *            feature group tag
     * @return map of features
     */
    private static Map<String, Feature> parseFeatureGroupTag(Element featGroupTag) {
        NamedNodeMap nnm = featGroupTag.getAttributes();
        String groupName;
        if (nnm.getNamedItem(FEATUREGROUP_ATTNAME) == null) {
            throw new IllegalArgumentException("Error syntax in configuration featuregroup : must have 'name' attribute");
        }
        groupName = nnm.getNamedItem(FEATUREGROUP_ATTNAME).getNodeValue();

        Map<String, Feature> groupFeatures = new HashMap<String, Feature>();
        NodeList listOfFeat = featGroupTag.getElementsByTagName(FEATURE_TAG);
        for (int k = 0; k < listOfFeat.getLength(); k++) {
            Feature f = parseFeatureTag((Element) listOfFeat.item(k));
            // Insert feature into group
            f.setGroup(groupName);
            groupFeatures.put(f.getUid(), f);
        }
        return groupFeatures;
    }

    /**
     * Build a Feature from XML TAG.
     * 
     * @param featXmlTag
     *            xml tag to nuild feature
     * @return current feature
     */
    private static Feature parseFeatureTag(Element featXmlTag) {
        NamedNodeMap nnm = featXmlTag.getAttributes();
        // Identifier
        String uid;
        if (nnm.getNamedItem(FEATURE_ATT_UID) == null) {
            throw new IllegalArgumentException(ERROR_SYNTAX_IN_CONFIGURATION_FILE + "'uid' is required for each feature");
        }
        uid = nnm.getNamedItem(FEATURE_ATT_UID).getNodeValue();
        // Enable
        if (nnm.getNamedItem(FEATURE_ATT_ENABLE) == null) {
            throw new IllegalArgumentException(ERROR_SYNTAX_IN_CONFIGURATION_FILE
                    + "'enable' is required for each feature (check " + uid + ")");
        }
        boolean enable = 
                Boolean.parseBoolean(nnm.getNamedItem(FEATURE_ATT_ENABLE).getNodeValue());

        // Create Feature with description
        Feature f = new Feature(uid).toggle(enable)
                                    .withDescription(parseDescription(nnm));
        
        /* Strategy
        NodeList flipStrategies = featXmlTag.getElementsByTagName(FLIPSTRATEGY_TAG);
        if (flipStrategies.getLength() > 0) {
            f.setFlippingStrategy(parseFlipStrategy((Element) flipStrategies.item(0), f.getUid()));
        }*/
        
        // Properties
        NodeList properties = featXmlTag.getElementsByTagName(PROPERTIES_CUSTOM_TAG);
        if (properties.getLength() > 0) {
            f.setCustomProperties(parsePropertiesTag((Element) properties.item(0)));
        }

        return f;
    }
    
    /**
     * Parse Properties.
     *
     * @param properties tag
     * @param uid
     *      current featureid
     * @return
     *      properties map
     */
    private static Map < String , Property<?>> parsePropertiesTag(Element propertiesTag) {
        Map< String , Property<?>> properties = new HashMap<String, Property<?>>(); 
        // <properties>
        NodeList lisOfProperties = propertiesTag.getElementsByTagName(PROPERTY_TAG);
        for (int k = 0; k < lisOfProperties.getLength(); k++) {
            // <property name='' value='' (type='') >
            Element propertyTag = (Element) lisOfProperties.item(k);
            NamedNodeMap attMap = propertyTag.getAttributes();
            if (attMap.getNamedItem(PROPERTY_PARAMNAME) == null) {
                throw new IllegalArgumentException("Invalid XML Syntax, 'name' is a required attribute of 'property' TAG");
            }
            if (attMap.getNamedItem(PROPERTY_PARAMVALUE) == null) {
                throw new IllegalArgumentException("Invalid XML Syntax, 'value' is a required attribute of 'property' TAG");
            }
            String name  = attMap.getNamedItem(PROPERTY_PARAMNAME).getNodeValue();
            String value = attMap.getNamedItem(PROPERTY_PARAMVALUE).getNodeValue();
            Property<?> ap = new PropertyString(name, value);
            
            // If specific type defined ?
            if (null != attMap.getNamedItem(PROPERTY_PARAMTYPE)) {
                String optionalType = attMap.getNamedItem(PROPERTY_PARAMTYPE).getNodeValue();
               
                // Substitution if relevant (e.g. 'int' -> 'org.ff4j.property.PropertyInt')
                optionalType = Property.mapPropertyType(optionalType);
                
                try {
                    // Constructor (String, String) is mandatory in Property interface
                    Constructor<?> constr = Class.forName(optionalType).getConstructor(String.class, String.class);
                    ap = (Property<?>) constr.newInstance(name, value);
                } catch (Exception e) {
                    throw new IllegalArgumentException("Cannot instantiate '" + optionalType + "' check default constructor", e);
                }
            }
            
            if (null != attMap.getNamedItem(PROPERTY_PARAMDESCRIPTION)) {
                ap.setDescription(attMap.getNamedItem(PROPERTY_PARAMDESCRIPTION).getNodeValue());
            }
            
            // Is there any fixed Value ?
            NodeList listOfFixedValue = propertyTag.getElementsByTagName(PROPERTY_PARAMFIXED_VALUES);
            if (listOfFixedValue.getLength() != 0) {
                Element fixedValueTag = (Element) listOfFixedValue.item(0);
                NodeList listOfValues =  fixedValueTag.getElementsByTagName(PROPERTY_PARAMVALUE);
                for (int l = 0; l < listOfValues.getLength(); l++) {
                    Element valueTag = (Element) listOfValues.item(l);
                    ap.add2FixedValueFromString(valueTag.getTextContent());
                }
            }
            
            // Check fixed value
            if (ap.getFixedValues().isPresent() &&  !ap.getFixedValues().get().contains(ap.getValue())) {
                throw new IllegalArgumentException("Cannot create property <" + ap.getUid() + 
                        "> invalid value <" + ap.getValue() + 
                        "> expected one of " + ap.getFixedValues());
            }
            
            properties.put(name, ap);
        }
        return properties;
    }

    /**
     * Parsing strategy TAG.
     * 
     * @param nnm
     *            current parend node
     * @param uid
     *            current feature uid
     * @return flipstrategy related to current feature.
     */
    @SuppressWarnings("unused")
    private static ToggleStrategy parseFlipStrategy(Element flipStrategyTag, String uid) {
        NamedNodeMap nnm = flipStrategyTag.getAttributes();
        ToggleStrategy flipStrategy;
        if (nnm.getNamedItem(TOGGLE_STRATEGY_ATTCLASS) == null) {
            throw new IllegalArgumentException("Error syntax in configuration file : '" + TOGGLE_STRATEGY_ATTCLASS
                    + "' is required for each flipstrategy (feature=" + uid + ")");
        }

        try {
            // Attribute CLASS
            String clazzName = nnm.getNamedItem(TOGGLE_STRATEGY_ATTCLASS).getNodeValue();
            flipStrategy = (ToggleStrategy) Class.forName(clazzName).newInstance();

            // LIST OF PARAMS
            Map<String, String> parameters = new LinkedHashMap<String, String>();
            NodeList initparamsNodes = flipStrategyTag.getElementsByTagName(TOGGLE_STRATEGY_PARAMTAG);
            for (int k = 0; k < initparamsNodes.getLength(); k++) {
                Element param = (Element) initparamsNodes.item(k);
                NamedNodeMap nnmap = param.getAttributes();
                // Check for required attribute name
                String currentParamName;
                if (nnmap.getNamedItem(TOGGLE_STRATEGY_PARAMNAME) == null) {
                    throw new IllegalArgumentException(ERROR_SYNTAX_IN_CONFIGURATION_FILE
                            + "'name' is required for each param in flipstrategy(check " + uid + ")");
                }
                currentParamName = nnmap.getNamedItem(TOGGLE_STRATEGY_PARAMNAME).getNodeValue();
                // Check for value attribute
                if (nnmap.getNamedItem(TOGGLE_STRATEGY_PARAMVALUE) != null) {
                    parameters.put(currentParamName, nnmap.getNamedItem(TOGGLE_STRATEGY_PARAMVALUE).getNodeValue());
                } else if (param.getFirstChild() != null) {
                    parameters.put(currentParamName, param.getFirstChild().getNodeValue());
                } else {
                    throw new IllegalArgumentException("Parameter '" + currentParamName + "' in feature '" + uid
                            + "' has no value, please check XML");
                }
            }

            flipStrategy.init(uid, parameters);
        } catch (Exception e) {
            throw new IllegalArgumentException("An error occurs during flipstrategy parsing TAG" + uid, e);
        }
        return flipStrategy;
    }

    /**
     * Parser target description.
     * 
     * @param nnm
     *            current working tag
     * @return description of the feature
     */
    private static String parseDescription(NamedNodeMap nnm) {
        String desc = null;
        if (nnm.getNamedItem(FEATURE_ATT_DESC) != null) {
            desc = nnm.getNamedItem(FEATURE_ATT_DESC).getNodeValue();
        }
        return desc;
    }

    /**
     * Build {@link DocumentBuilder} to parse XML.
     * 
     * @return current document builder.
     * @throws ParserConfigurationException
     *             error during initialization
     */
    public static DocumentBuilder getDocumentBuilder() throws ParserConfigurationException {
        if (builder == null) {
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            // -- Prevent against XXE @see https://www.owasp.org/index.php/XML_External_Entity_(XXE)_Processing
            dbf.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
            // If you can't completely disable DTDs, then at least do the following:
            // Xerces 1 - http://xerces.apache.org/xerces-j/features.html#external-general-entities
            // Xerces 2 - http://xerces.apache.org/xerces2-j/features.html#external-general-entities
            // JDK7+ - http://xml.org/sax/features/external-general-entities    
            dbf.setFeature("http://xml.org/sax/features/external-general-entities", false);
            // Xerces 1 - http://xerces.apache.org/xerces-j/features.html#external-parameter-entities
            // Xerces 2 - http://xerces.apache.org/xerces2-j/features.html#external-parameter-entities
            // JDK7+ - http://xml.org/sax/features/external-parameter-entities    
            dbf.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
            // Disable external DTDs as well
            dbf.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
            // and these as well, per Timothy Morgan's 2014 paper: "XML Schema, DTD, and Entity Attacks" (see reference below)
            dbf.setXIncludeAware(false);
            dbf.setExpandEntityReferences(false);
            builder = dbf.newDocumentBuilder();
            builder.setErrorHandler(new XmlParserErrorHandler());
        }
        return builder;
    }

    /**
     * Create XML output stream from a map of {@link Feature}.
     * 
     * @param mapOfFeatures
     *            map of features
     * @return streams
     * @throws IOException
     *             error occurs when generating output
     */
    public static InputStream exportFeatures(Stream < Feature> mapOfFeatures) throws IOException {    
        return new ByteArrayInputStream(exportFeaturesPart(mapOfFeatures).getBytes(ENCODING));
    }
    
    /**
     * Create XML output stream from a map of {@link PropertyString}.
     * 
     * @param mapOfProperties
     *            map of properties
     * @return streams
     * @throws IOException
     *             error occurs when generating output
     */
    public static InputStream exportProperties(Stream < Property<?> > mapOfProperties) throws IOException {   
        return new ByteArrayInputStream(exportPropertiesPart(mapOfProperties).getBytes(ENCODING));
    }
    
    /**
     * Create XML output stream with both {@link Feature} and {@link PropertyString}.
     * 
     * @param f
     *            map of features
     * @return streams
     * @throws IOException
     *             error occurs when generating output
     */
    public static InputStream exportAll(Map<String, Feature> mapOfFeatures, Map < String, Property<?>> mapOfProperties) throws IOException {   
        // Create output
        StringBuilder sb = new StringBuilder(XML_HEADER);
        sb.append(exportFeaturesPart(mapOfFeatures.values().stream()));
        sb.append(exportPropertiesPart(mapOfProperties.values().stream()));
        sb.append(END_FF4J);
        return new ByteArrayInputStream(sb.toString().getBytes(ENCODING));
    }
    
    /**
     * Utility method to export from configuration.
     *
     * @param conf
     *      target configuration
     * @return
     *      target stream
     * @throws IOException
     *      error during marshalling
     */
    public static InputStream exportAll(FF4jConfigFile conf) throws IOException {
        return exportAll(conf.getFeatures(), conf.getProperties());
    }
    
    /**
     * Create dedicated output for Properties.
     *
     * @param mapOfProperties
     *      target properties
     * @return
     *      XML Flow     
     */
    private static String exportPropertiesPart(Stream < Property<?> > streamOfProperties) {
        StringBuilder sb = new StringBuilder(BEGIN_PROPERTIES);
        sb.append(buildPropertiesPart(streamOfProperties));
        sb.append(END_PROPERTIES);
        return sb.toString();
    }
    
    /**
     * Export Features part of the XML.
     *
     * @param mapOfFeatures
     *      current map of feaures.
     * 
     * @return
     *      all XML
     */
    private static String exportFeaturesPart(Stream<Feature> streamOfFeatures) {
        // Split features
        Map<String, List<Feature>> mapOfGroups = new HashMap<String, List<Feature>>();
        List < Feature > noGroupFeatures = new ArrayList<>();
        streamOfFeatures.forEach(feature -> {
            if (feature.getGroup().isPresent()) {
                String groupName = feature.getGroup().get();
                if (!mapOfGroups.containsKey(groupName)) {
                    mapOfGroups.put(groupName, new ArrayList<Feature>());
                }
                mapOfGroups.get(groupName).add(feature);
            } else {
                noGroupFeatures.add(feature);
            }
        });
            
        // Create <features>
        StringBuilder sb = new StringBuilder(BEGIN_FEATURES);
        
        mapOfGroups.entrySet().stream().forEach(g -> {
            sb.append(" <" + FEATUREGROUP_TAG + " " + FEATUREGROUP_ATTNAME + "=\"" + g.getKey() + "\" >\n\n");
            g.getValue().stream().forEach(feature -> sb.append(exportFeature(feature)));
            sb.append(" </" + FEATUREGROUP_TAG + ">\n\n");
        });
        
        noGroupFeatures.stream().forEach(feature -> sb.append(exportFeature(feature)));
        
        sb.append(END_FEATURES);
        return sb.toString();
    }
    
    private static String exportFeature(Feature feature) {
        StringBuilder sb = new StringBuilder();
        
        // <feature uid=.. enable=... <description=...> 
        sb.append(MessageFormat.format(XML_FEATURE, feature.getUid(), feature.isEnable()));
        feature.getDescription().ifPresent(desc -> sb.append(" description=\"" + desc + "\""));
        sb.append(" >\n");
        
        /* <flipstrategy>
        if (feature.getFlippingStrategy().isPresent()) {
            ToggleStrategy fs = feature.getFlippingStrategy().get();
            sb.append("   <" + FLIPSTRATEGY_TAG + " class=\"" + fs.getClass().getCanonicalName() + "\" >\n");
            for (String p : fs.getInitParams().keySet()) {
                sb.append("     <" + FLIPSTRATEGY_PARAMTAG + " " + FLIPSTRATEGY_PARAMNAME + "=\"");
                sb.append(p);
                sb.append("\" " + FLIPSTRATEGY_PARAMVALUE + "=\"");
                // Escape special characters to build XML
                // https://github.com/clun/ff4j/issues/63
                String paramValue = fs.getInitParams().get(p);
                sb.append(escapeXML(paramValue));
                sb.append("\" />\n");
            }
            sb.append("   </" + FLIPSTRATEGY_TAG + ">\n");
        }*/
        
        // <custom-properties>
        if (feature.getCustomProperties().isPresent()) {
            sb.append(BEGIN_CUSTOMPROPERTIES);
            sb.append(buildPropertiesPart(feature.getCustomProperties().get().values().stream()));
            sb.append(END_CUSTOMPROPERTIES);
        }
        sb.append(END_FEATURE);
        return sb.toString();
    }
    
    /**
     * Create XML content of the properties or custom properties elements.
     *
     * @param props
     *      properties elements.
     * @return
     */
    private static String buildPropertiesPart(Stream < Property<?> > props) {
        final StringBuilder sb = new StringBuilder();
        if (props != null) {
            props.forEach(property -> {
                sb.append("    <" + PROPERTY_TAG + " " + PROPERTY_PARAMNAME + "=\"" + property.getUid() + "\" ");
                sb.append(PROPERTY_PARAMVALUE + "=\"" + property.asString() + "\" ");
                if (!(property instanceof PropertyString)) {
                    sb.append(PROPERTY_PARAMTYPE  + "=\"" + property.getClass().getCanonicalName()  + "\"");
                }
                // Processing fixedValue is present
                if (property.getFixedValues().isPresent()) {
                    sb.append(">\n");
                    sb.append("     <fixedValues>\n");
                    property.getFixedValues().get().stream()
                        .forEach(o -> sb.append("      <value>" + o.toString() + "</value>\n"));
                    sb.append("     </fixedValues>\n");
                    sb.append("    </property>\n");
                } else {
                    sb.append("/>\n");
                }
            });
        }
        return sb.toString();
    }
   
    /**
     * Substitution to create XML.
     *
     * @param value
     *      target XML
     * @return
     */
    public static String escapeXML(String value) {
        if (value == null) {
            return null;
        }
        return value.replaceAll("&", "&amp;")
                    .replaceAll(">", "&gt;")
                    .replaceAll("<", "&lt;");
    }
    

}
