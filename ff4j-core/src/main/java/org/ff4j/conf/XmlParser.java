package org.ff4j.conf;

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
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set; 
import java.util.TreeSet;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.ff4j.core.Feature;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyString;
import org.ff4j.utils.MappingUtil;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;

/**
 * Allow to parse XML files to load {@link Feature}.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public final class XmlParser {

    /** TAG XML. */
    public static final String FEATURES_TAG = "features";

    /** TAG XML. */
    public static final String FEATURE_TAG = "feature";

    /** TAG XML. */
    public static final String FEATURE_ATT_UID = "uid";

    /** TAG XML. */
    public static final String FEATURE_ATT_DESC = "description";

    /** TAG XML. */
    public static final String FEATURE_ATT_ENABLE = "enable";

    /** TAG XML. */
    public static final String FEATUREGROUP_TAG = "feature-group";

    /** TAG XML. */
    public static final String FEATUREGROUP_ATTNAME = "name";

    /** TAG XML. */
    public static final String FLIPSTRATEGY_TAG = "flipstrategy";
    
    /** TAG XML. */
    public static final String PROPERTIES_TAG = "properties";
    
    /** TAG XML. */
    public static final String PROPERTIES_CUSTOM_TAG = "custom-properties";
    
    /** TAG XML. */
    public static final String PROPERTY_TAG = "property";
    
    /** TAG XML. */
    public static final String PROPERTY_PARAMTYPE = "type";
    
    /** TAG XML. */
    public static final String PROPERTY_PARAMNAME = "name";
    
    /** TAG XML. */
    public static final String PROPERTY_PARAMDESCRIPTION = "description";

    /** TAG XML. */
    public static final String PROPERTY_PARAMVALUE = "value";
    
    /** TAG XML. */
    public static final String PROPERTY_PARAMFIXED_VALUES = "fixedValues";
    
    /** TAG XML. */
    public static final String FLIPSTRATEGY_ATTCLASS = "class";

    /** TAG XML. */
    public static final String FLIPSTRATEGY_PARAMTAG = "param";

    /** TAG XML. */
    public static final String FLIPSTRATEGY_PARAMNAME = "name";

    /** TAG XML. */
    public static final String FLIPSTRATEGY_PARAMVALUE = "value";

    /** TAG XML. */
    public static final String SECURITY_TAG = "security";

    /** TAG XML. */
    public static final String SECURITY_ROLE_TAG = "role";

    /** TAG XML. */
    public static final String SECURITY_ROLE_ATTNAME = "name";

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
            + "\n xsi:schemaLocation=\"http://www.ff4j.org/schema/ff4j http://ff4j.org/schema/ff4j-1.4.0.xsd\">"
            + ">\n\n";

    /** XML Generation constants. */
    private static final String XML_FEATURE = "  <feature uid=\"{0}\" description=\"{1}\" enable=\"{2}\">\n";

    /** XML Generation constants. */
    private static final String XML_AUTH = "      <role name=\"{0}\" />\n";

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
    public static final String ERROR_SYNTAX_IN_CONFIGURATION_FILE = "Error syntax in configuration file : ";

    /** Document Builder use to parse XML. */
    private static DocumentBuilder builder = null;
   
    /**
     * Parsing of XML Configuration file.
     *
     * @param file
     *      target file
     * @return
     *      features and properties find within file
     */
    public XmlConfig parseConfigurationFile(InputStream in) {
        try {
            
            // Object to be build by parsing
            XmlConfig xmlConf = new XmlConfig();
                
            // Load XML as a Document
            Document ff4jDocument = getDocumentBuilder().parse(in);
            
            // Features Tag
            NodeList fList = ff4jDocument.getElementsByTagName(FEATURES_TAG);
            if (fList.getLength() > 1) {
                throw new IllegalArgumentException("Root Tag is 'features' and must be unique, please check");
            } else if (fList.getLength() == 1) {
                xmlConf.setFeatures(parseFeaturesTag( (Element) fList.item(0)));
            }
            
            // Properties Tag
            NodeList pList = ff4jDocument.getElementsByTagName(PROPERTIES_TAG);
            if (pList.getLength() > 1) {
                throw new IllegalArgumentException("Root Tag is 'properties' and must be unique, please check");
            } else if (pList.getLength() == 1) {
               xmlConf.setProperties(parsePropertiesTag((Element) pList.item(0)));
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
    public Map<String, Feature> parseFeaturesTag(Element featuresTag) {
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
    private Map<String, Feature> parseFeatureGroupTag(Element featGroupTag) {
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
    private Feature parseFeatureTag(Element featXmlTag) {
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
        boolean enable = Boolean.parseBoolean(nnm.getNamedItem(FEATURE_ATT_ENABLE).getNodeValue());

        // Create Feature with description
        Feature f = new Feature(uid, enable, parseDescription(nnm));
        
        // Strategy
        NodeList flipStrategies = featXmlTag.getElementsByTagName(FLIPSTRATEGY_TAG);
        if (flipStrategies.getLength() > 0) {
            f.setFlippingStrategy(parseFlipStrategy((Element) flipStrategies.item(0), f.getUid()));
        }
        
        // Security
        NodeList securities = featXmlTag.getElementsByTagName(SECURITY_TAG);
        if (securities.getLength() > 0) {
            f.setPermissions(parseListAuthorizations((Element) securities.item(0)));
        }
        
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
    private Map < String , Property<?>> parsePropertiesTag(Element propertiesTag) {
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
                optionalType = MappingUtil.mapPropertyType(optionalType);
                
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
            if (ap.getFixedValues() != null && !ap.getFixedValues().contains(ap.getValue())) {
                throw new IllegalArgumentException("Cannot create property <" + ap.getName() + 
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
    private FlippingStrategy parseFlipStrategy(Element flipStrategyTag, String uid) {
        NamedNodeMap nnm = flipStrategyTag.getAttributes();
        FlippingStrategy flipStrategy;
        if (nnm.getNamedItem(FLIPSTRATEGY_ATTCLASS) == null) {
            throw new IllegalArgumentException("Error syntax in configuration file : '" + FLIPSTRATEGY_ATTCLASS
                    + "' is required for each flipstrategy (feature=" + uid + ")");
        }

        try {
            // Attribute CLASS
            String clazzName = nnm.getNamedItem(FLIPSTRATEGY_ATTCLASS).getNodeValue();
            flipStrategy = (FlippingStrategy) Class.forName(clazzName).newInstance();

            // LIST OF PARAMS
            Map<String, String> parameters = new LinkedHashMap<String, String>();
            NodeList initparamsNodes = flipStrategyTag.getElementsByTagName(FLIPSTRATEGY_PARAMTAG);
            for (int k = 0; k < initparamsNodes.getLength(); k++) {
                Element param = (Element) initparamsNodes.item(k);
                NamedNodeMap nnmap = param.getAttributes();
                // Check for required attribute name
                String currentParamName;
                if (nnmap.getNamedItem(FLIPSTRATEGY_PARAMNAME) == null) {
                    throw new IllegalArgumentException(ERROR_SYNTAX_IN_CONFIGURATION_FILE
                            + "'name' is required for each param in flipstrategy(check " + uid + ")");
                }
                currentParamName = nnmap.getNamedItem(FLIPSTRATEGY_PARAMNAME).getNodeValue();
                // Check for value attribute
                if (nnmap.getNamedItem(FLIPSTRATEGY_PARAMVALUE) != null) {
                    parameters.put(currentParamName, nnmap.getNamedItem(FLIPSTRATEGY_PARAMVALUE).getNodeValue());
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
     * Parsing autorization tag.
     * 
     * @param featXmlTag
     *            current TAG
     * @return list of authorizations.
     */
    private static Set<String> parseListAuthorizations(Element securityTag) {
        Set<String> authorizations = new TreeSet<String>();
        NodeList lisOfAuth = securityTag.getElementsByTagName(SECURITY_ROLE_TAG);
        for (int k = 0; k < lisOfAuth.getLength(); k++) {
            Element role = (Element) lisOfAuth.item(k);
            authorizations.add(role.getAttributes().getNamedItem(SECURITY_ROLE_ATTNAME).getNodeValue());
        }
        return authorizations;
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
    public InputStream exportFeatures(Map<String, Feature> mapOfFeatures) throws IOException {    
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
    public InputStream exportProperties(Map < String, Property<?>> mapOfProperties) throws IOException {   
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
    public InputStream exportAll(Map<String, Feature> mapOfFeatures, Map < String, Property<?>> mapOfProperties) throws IOException {   
        // Create output
        StringBuilder sb = new StringBuilder(XML_HEADER);
        sb.append(exportFeaturesPart(mapOfFeatures));
        sb.append(exportPropertiesPart(mapOfProperties));
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
    public InputStream exportAll(XmlConfig conf) throws IOException {
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
    private String exportPropertiesPart(Map < String, Property<?>> mapOfProperties) {
        // Create <features>
        StringBuilder sb = new StringBuilder(BEGIN_PROPERTIES);
        if (mapOfProperties != null && !mapOfProperties.isEmpty()) {
            sb.append(buildPropertiesPart(mapOfProperties));
        }
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
    private String exportFeaturesPart(Map<String, Feature> mapOfFeatures) {
        // Create <features>
        StringBuilder sb = new StringBuilder(BEGIN_FEATURES);
        
        // Recreate Groups
        Map<String, List<Feature>> featuresPerGroup = new HashMap<String, List<Feature>>();
        if (mapOfFeatures != null && !mapOfFeatures.isEmpty()) {
            for (Feature feat : mapOfFeatures.values()) {
                String groupName = feat.getGroup();
                if (!featuresPerGroup.containsKey(groupName)) {
                    featuresPerGroup.put(groupName, new ArrayList<Feature>());
                }
                featuresPerGroup.get(groupName).add(feat);
            }
        }
            
        for (Map.Entry<String,List<Feature>> groupName : featuresPerGroup.entrySet()) {
            /// Building featureGroup
            if (null != groupName.getKey() && !groupName.getKey().isEmpty()) {
                sb.append(" <" + FEATUREGROUP_TAG + " " + FEATUREGROUP_ATTNAME + "=\"" + groupName.getKey() + "\" >\n\n");
            }
            // Loop on feature
            for (Feature feat : groupName.getValue()) {
                sb.append(MessageFormat.format(XML_FEATURE, feat.getUid(), feat.getDescription(), feat.isEnable()));
                // <security>
                if (null != feat.getPermissions() && !feat.getPermissions().isEmpty()) {
                    sb.append("   <" + SECURITY_TAG + ">\n");
                    for (String auth : feat.getPermissions()) {
                        sb.append(MessageFormat.format(XML_AUTH, auth));
                    }
                    sb.append("   </" + SECURITY_TAG + ">\n");
                }
                // <flipstrategy>
                FlippingStrategy fs = feat.getFlippingStrategy();
                if (null != fs) {
                    sb.append("   <" + FLIPSTRATEGY_TAG + " class=\"" + fs.getClass().getName() + "\" >\n");
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
                }
                // <custom-properties>
                Map < String, Property<?>> props = feat.getCustomProperties();
                if (props != null && !props.isEmpty()) {
                    sb.append(BEGIN_CUSTOMPROPERTIES);
                    sb.append(buildPropertiesPart(feat.getCustomProperties()));
                    sb.append(END_CUSTOMPROPERTIES);
                }
                sb.append(END_FEATURE);
            }
            
            if (null != groupName.getKey() && !groupName.getKey().isEmpty()) {
                sb.append(" </" + FEATUREGROUP_TAG + ">\n\n");
            }
        }
        sb.append(END_FEATURES);
        return sb.toString();
    }
    
    /**
     * Create XML content of the properties or custom properties elements.
     *
     * @param props
     *      properties elements.
     * @return
     */
    private String buildPropertiesPart(Map < String, Property<?>> props) {
        StringBuilder sb = new StringBuilder();
        if (props != null && !props.isEmpty()) {
            // Loop over property
            for (Property<?> property : props.values()) {
                sb.append("    <" + PROPERTY_TAG + " " + PROPERTY_PARAMNAME + "=\"" + property.getName() + "\" ");
                sb.append(PROPERTY_PARAMVALUE + "=\"" + property.asString() + "\" ");
                if (!(property instanceof PropertyString)) {
                    sb.append(PROPERTY_PARAMTYPE  + "=\"" + property.getClass().getName()  + "\"");
                }
                // Processing fixedValue is present
                if (property.getFixedValues() != null && !property.getFixedValues().isEmpty()) {
                    sb.append(">\n");
                    sb.append("     <fixedValues>\n");
                    for (Object o : property.getFixedValues()) {
                        sb.append("      <value>" + o.toString() + "</value>\n");
                    }
                    sb.append("     </fixedValues>\n");
                    sb.append("    </property>\n");
                } else {
                    sb.append("/>\n");
                }
            }
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
    public String escapeXML(String value) {
        if (value == null) {
            return null;
        }
        return value.replaceAll("&", "&amp;")
                    .replaceAll(">", "&gt;")
                    .replaceAll("<", "&lt;");
    }
    

}
