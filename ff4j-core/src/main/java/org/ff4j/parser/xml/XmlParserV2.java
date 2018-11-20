package org.ff4j.parser.xml;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Constructor;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.ff4j.feature.Feature;
import org.ff4j.feature.strategy.TogglePredicate;
import org.ff4j.parser.FF4jConfigFile;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyString;
import org.ff4j.security.FF4jPermission;
import org.ff4j.test.AssertUtils;
import org.ff4j.user.FF4jRole;
import org.ff4j.user.FF4jUser;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

/**
 * Allow to parse XML files to load {@link Feature}.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public final class XmlParserV2 extends ConfigurationFileParserXml {
   
    /** Document Builder use to parse XML. */
    private static DocumentBuilder builder = null;
    
    /**  Hide constructor. */
    public XmlParserV2() {
    }
    
    /**
     * Parsing of XML Configuration file.
     *
     * @param file
     *      target file
     * @return
     *      features and properties find within file
     */
    @Override
    public FF4jConfigFile parse(InputStream in) {
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
    private static Map<String, FF4jRole > parseRolesTag(Element rolesTag) {
        Map<String, FF4jRole> mapOfRoles = new HashMap<>();
        NodeList roleNodes = rolesTag.getChildNodes();
        for (int i = 0; i < roleNodes.getLength(); i++) {
            if (roleNodes.item(i) instanceof Element) {
                parseRoleTag((Element) roleNodes.item(i), mapOfRoles);
            }
        }
        return mapOfRoles;
    }
    
    private static void parseRoleTag(Element roleNode, Map<String, FF4jRole > mapOfRoles) {
        NamedNodeMap nnm = roleNode.getAttributes();
        Node attributeName = nnm.getNamedItem(SECURITY_ROLE_ATTNAME);
        AssertUtils.assertNotNull(attributeName);
        Set<FF4jPermission> setOfPermissions = parseRolePermissionsTag(roleNode);
        FF4jRole role = new FF4jRole(attributeName.getNodeValue());
        role.grant(setOfPermissions.toArray(new FF4jPermission[0]));
        mapOfRoles.put(attributeName.getNodeValue(), role);
    }
    
    private static Set<FF4jPermission> parseRolePermissionsTag(Element roleNode) {
        Set<FF4jPermission> setOfPermissions = new HashSet<>();
        NodeList permissionNodes = roleNode.getChildNodes();
        for (int j = 0; j < permissionNodes.getLength(); j++) {
            if (permissionNodes.item(j) instanceof Element) {
                Element permissionNode = (Element) permissionNodes.item(j);
                setOfPermissions.add(FF4jPermission.valueOf(permissionNode.getTextContent().trim()));
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
                                    .description(parseDescription(nnm));
        
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
    private static TogglePredicate parseFlipStrategy(Element flipStrategyTag, String uid) {
        NamedNodeMap nnm = flipStrategyTag.getAttributes();
        TogglePredicate flipStrategy;
        if (nnm.getNamedItem(TOGGLE_STRATEGY_ATTCLASS) == null) {
            throw new IllegalArgumentException("Error syntax in configuration file : '" + TOGGLE_STRATEGY_ATTCLASS
                    + "' is required for each flipstrategy (feature=" + uid + ")");
        }

        try {
            // Attribute CLASS
            String clazzName = nnm.getNamedItem(TOGGLE_STRATEGY_ATTCLASS).getNodeValue();
            flipStrategy = (TogglePredicate) Class.forName(clazzName).newInstance();

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


}
