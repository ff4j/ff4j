package org.ff4j.parser.xml;

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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

import org.ff4j.feature.Feature;
import org.ff4j.parser.ConfigurationFileParser;
import org.ff4j.parser.FF4jConfigFile;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyString;
import org.ff4j.utils.Util;

/**
 * In v2 we still need to read v1 files (to convert to v2)
 * 
 * @author Cedrick LUNVEN (@clunven)
 */
public abstract class ConfigurationFileParserXml extends ConfigurationFileParser {
    
    /** TAG XML. */
    public static final String CDATA_START = "<![CDATA[";

    /** TAG XML. */
    public static final String CDATA_END = "]]>";

    /** XML Generation constants. */
    protected static final String ENCODING = "UTF-8";

    /** XML Generation constants. */
    protected static final String XML_HEADER = 
            "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"//
            + "<ff4j xmlns=\"http://www.ff4j.org/schema/ff4j\""//
            + "\n xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\""//
            + "\n xsi:schemaLocation=\"http://www.ff4j.org/schema/ff4j http://ff4j.org/schema/ff4j-2.0.xsd\">"
            + ">\n\n";

    /** XML Generation constants. */
    protected static final String XML_FEATURE = "  <feature uid=\"{0}\" enable=\"{1}\" ";

    /** XML Generation constants. */
    protected static final String END_FEATURE = "  </feature>\n\n";

    /** XML Generation constants. */
    protected static final String BEGIN_FEATURES = " <features>\n\n";
    
    /** XML Generation constants. */
    protected static final String END_FEATURES = " </features>\n\n";
    
    /** XML Generation constants. */
    protected static final String BEGIN_PROPERTIES = " <properties>\n\n";
    
    /** XML Generation constants. */
    protected static final String BEGIN_CUSTOMPROPERTIES = "   <custom-properties>\n";
    
    /** XML Generation constants. */
    protected static final String END_CUSTOMPROPERTIES = "   </custom-properties>\n";
    
    /** XML Generation constants. */
    protected static final String END_PROPERTIES = " </properties>\n\n";
    
    /** XML Generation constants. */
    protected static final String END_FF4J = "</ff4j>\n\n";
    
    /** Error message. */
    public static final String ERROR_SYNTAX_IN_CONFIGURATION_FILE = "Error syntax in configuration file : ";
    
    /** {@inheritDoc} */
    @Override
    public String export(FF4jConfigFile config) {
        try {
            return Util.fromInputStreamToString(exportAll(config));
        } catch (IOException e) {
           throw new IllegalArgumentException("Cannot export configuration as file", e);
        }
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
        sb.append(MessageFormat.format(XML_FEATURE, feature.getUid(), feature.isEnabled()));
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
        if (!feature.getProperties().isEmpty()) {
            sb.append(BEGIN_CUSTOMPROPERTIES);
            sb.append(buildPropertiesPart(feature.getProperties().values().stream()));
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
