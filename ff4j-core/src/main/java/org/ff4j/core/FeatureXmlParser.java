package org.ff4j.core;

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

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

/**
 * Allow to parse XML files to load {@link Feature}.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public final class FeatureXmlParser {

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
            + "<features xmlns=\"http://www.ff4j.org/schema/ff4j\""//
            + "\n xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\""//
            + "\n xsi:schemaLocation=\"http://www.ff4j.org/schema/ff4j http://ff4j.org/schema/ff4j-1.2.0.xsd\">"
            + ">\n\n";

    /** XML Generation constants. */
    private static final String XML_FEATURE = " <feature uid=\"{0}\" description=\"{1}\" enable=\"{2}\">\n";

    /** XML Generation constants. */
    private static final String XML_AUTH = "     <role name=\"{0}\" />\n";

    /** XML Generation constants. */
    private static final String END_FEATURE = " </feature>\n\n";

    /** XML Generation constants. */
    private static final String END_FEATURES = "</features>\n\n";

    /** Document Builder use to parse XML. */
    private static DocumentBuilder builder = null;
   
    /**
     * Load map of {@link Feature} from an inpustream (containing xml text).
     * 
     * @param in
     *            inpustream with XML text
     * @return the sorted map of features
     * @throws IOException
     *             exception raised when reading inputstream
     */
    public Map<String, Feature> parseConfigurationFile(InputStream in) {
        LinkedHashMap<String, Feature> xmlFeatures = new LinkedHashMap<String, Feature>();
        try {
            // Load XML as a Document
            Document ff4jDocument = getDocumentBuilder().parse(in);

            // Features Tag
            NodeList rootList = ff4jDocument.getElementsByTagName(FEATURES_TAG);
            if (rootList.getLength() != 1) {
                throw new IllegalArgumentException("Root Tag is 'features' and must be unique, please check");
            }
            Element featuresTag = (Element) rootList.item(0);

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
        } catch (IOException e) {
            throw new IllegalArgumentException("Cannot parse XML data, please check file access ", e);
        } catch (ParserConfigurationException e1) {
            throw new IllegalArgumentException("Error during initialization of parser ", e1);
        } catch (SAXException e2) {
            throw new IllegalArgumentException("Cannot parse XML, invalid format ", e2);
        }
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
        String groupName = null;
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
        String uid = null;
        if (nnm.getNamedItem(FEATURE_ATT_UID) == null) {
            throw new IllegalArgumentException("Error syntax in configuration file : " + "'uid' is required for each feature");
        }
        uid = nnm.getNamedItem(FEATURE_ATT_UID).getNodeValue();
        // Enable
        if (nnm.getNamedItem(FEATURE_ATT_ENABLE) == null) {
            throw new IllegalArgumentException("Error syntax in configuration file : "
                    + "'enable' is required for each feature (check " + uid + ")");
        }
        boolean enable = Boolean.valueOf(nnm.getNamedItem(FEATURE_ATT_ENABLE).getNodeValue());

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

        return f;
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
        FlippingStrategy flipStrategy = null;
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
                String currentParamName = null;
                if (nnmap.getNamedItem(FLIPSTRATEGY_PARAMNAME) == null) {
                    throw new IllegalArgumentException("Error syntax in configuration file : "
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
    private String parseDescription(NamedNodeMap nnm) {
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
    private Set<String> parseListAuthorizations(Element securityTag) {
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
    public DocumentBuilder getDocumentBuilder() throws ParserConfigurationException {
        if (builder == null) {
            builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
            builder.setErrorHandler(new ErrorHandler() {
                /** {@inheritDoc} */
                @Override
                public void warning(SAXParseException e) throws SAXException {}

                /** {@inheritDoc} */
                @Override
                public void fatalError(SAXParseException e) throws SAXException {
                    throw e;
                }

                /** {@inheritDoc} */
                @Override
                public void error(SAXParseException e) throws SAXException {
                    throw e;
                }
            });
        }
        return builder;
    }

    /**
     * Create XML output stream from a map of {@link Feature}.
     * 
     * @param f
     *            map of features
     * @return streams
     * @throws IOException
     *             error occurs when generating output
     */
    public InputStream exportFeatures(Map<String, Feature> mapOfFeatures) throws IOException {
        if (mapOfFeatures != null && !mapOfFeatures.isEmpty()) {
            // Recreate Groups
            Map<String, List<Feature>> featuresPerGroup = new HashMap<String, List<Feature>>();
            for (Feature feat : mapOfFeatures.values()) {
                String groupName = feat.getGroup();
                if (!featuresPerGroup.containsKey(groupName)) {
                    featuresPerGroup.put(groupName, new ArrayList<Feature>());
                }
                featuresPerGroup.get(groupName).add(feat);
            }
            
            // Create output
            StringBuilder sb = new StringBuilder(XML_HEADER);
            for (String groupName : featuresPerGroup.keySet()) {
                
                /// Building featureGroup
                if (null != groupName && !groupName.isEmpty()) {
                    sb.append(" <" + FEATUREGROUP_TAG + " " + FEATUREGROUP_ATTNAME + "=\"" + groupName + "\" >\n\n");
                }
                // Loop on feature
                for (Feature feat : featuresPerGroup.get(groupName)) {
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
                        sb.append("   <" + FLIPSTRATEGY_TAG + " class=\"" + fs.getClass().getCanonicalName() + "\" >\n");
                        for (String p : fs.getInitParams().keySet()) {
                            sb.append("     <" + FLIPSTRATEGY_PARAMTAG + " " + FLIPSTRATEGY_PARAMNAME + "=\"");
                            sb.append(p);
                            sb.append("\" " + FLIPSTRATEGY_PARAMVALUE + "=\"");
                            sb.append(fs.getInitParams().get(p));
                            sb.append("\" />\n");
                        }
                        sb.append("   </" + FLIPSTRATEGY_TAG + ">\n");
                    }
                    sb.append(END_FEATURE);
                }
                
                if (null != groupName && !groupName.isEmpty()) {
                    sb.append(" </" + FEATUREGROUP_TAG + ">\n\n");
                }
            }
            sb.append(END_FEATURES);
            return new ByteArrayInputStream(sb.toString().getBytes(ENCODING));
        }
        return null;
    }


}
