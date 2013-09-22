package org.ff4j.core;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.ff4j.strategy.FlippingStrategy;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * Import Features from a well formated XML file.
 * 
 * <p>
 * Sample File :
 * <p>
 * 
 * <pre>
 * &lt;?xml version="1.0" encoding="UTF-8" ?&gt;
 * &lt;features&gt;
 *  &lt;feature uid="abc" description="voila la desctiption autocompletee" enabled="true" &gt;
 *   &lt;auth role="ROLE_USER" /&gt;
 *   &lt;auth role="ABD" /&gt;
 *  &lt;/feature&gt;
 *  &lt;feature uid="dddd" description="voila la desctiption autocompletee" enabled="false" /&lt;
 * &lt;/features&gt;
 * </pre>
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public final class FeatureLoader {

    /** XML Tags and attribute. */
    public static final String TAG_FEATURES = "features";

    /** XML Tags and attribute. */
    public static final String TAG_FEATURE = "feature";

    /** XML Tags and attribute. */
    public static final String TAG_AUTH = "auth";

    /** XML Tags and attribute. */
    public static final String ATT_UID = "uid";

    /** XML Tags and attribute. */
    public static final String ATT_DESC = "description";

    /** XML Tags and attribute. */
    public static final String ATT_ENABLE = "enable";

    /** XML Tags and attribute. */
    public static final String ATT_ROLE = "role";

    public static final String ATT_STRATEGY = "strategy";

    public static final String ATT_EXPRESSION = "expression";

    /** XML Generation constants. */
    private static final String ENCODING = "UTF-8";

    /** XML Generation constants. */
    private static final String XML_HEADER = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n<features>\n\n";

    /** XML Generation constants. */
    private static final String XML_FEATURE = " <feature uid=\"{0}\" description=\"{1}\" enabled=\"{2}\">\n";

    /** XML Generation constants. */
    private static final String XML_AUTH = "   <auth role=\"{0}\" />\n";

    /** XML Generation constants. */
    private static final String END_FEATURE = " </feature>\n\n";

    /** XML Generation constants. */
    private static final String END_FEATURES = "</features>";

    /**
     * As utility class, hide constructor.
     */
    private FeatureLoader() {}

    /**
     * Load map of {@link Feature} from an inpustream (containing xml text).
     * 
     * @param in
     *            inpustream with XML text
     * @return the sorted map of features
     * @throws IOException
     *             exception raised when reading inputstream
     */
    public static Map<String, Feature> loadFeatures(InputStream in) {
        LinkedHashMap<String, Feature> xmlFeatures = new LinkedHashMap<String, Feature>();
        try {
            Document featuresTag = DocumentBuilderFactory.newInstance().newDocumentBuilder().parse(in);
            if (featuresTag.hasChildNodes()) {
                NodeList listOfFeature = featuresTag.getElementsByTagName(TAG_FEATURE);
                for (int i = 0; i < listOfFeature.getLength(); i++) {
                    Feature f = parseFeature((Element) listOfFeature.item(i));
                    xmlFeatures.put(f.getUid(), f);
                }
            }
            return xmlFeatures;
        } catch (IOException e) {
            throw new IllegalArgumentException("Cannot parse XML data, please check file access ", e);
        } catch (ParserConfigurationException e1) {
            throw new IllegalArgumentException("Cannot parse XML data, please check source file ", e1);
        } catch (SAXException e2) {
            throw new IllegalArgumentException("Cannot parse XML, invalid format ", e2);
        }
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
    public static InputStream exportFeatures(Map<String, Feature> mapOfFeatures) throws IOException {
        StringBuilder strBuilder = new StringBuilder(XML_HEADER);
        if (mapOfFeatures != null && !mapOfFeatures.isEmpty()) {
            for (Feature feat : mapOfFeatures.values()) {
                strBuilder.append(MessageFormat.format(XML_FEATURE, feat.getUid(), feat.getDescription(), feat.isEnable()));
                if (feat.getAuthorizations() != null && !feat.getAuthorizations().isEmpty()) {
                    for (String auth : feat.getAuthorizations()) {
                        strBuilder.append(MessageFormat.format(XML_AUTH, auth));
                    }
                }
                strBuilder.append(END_FEATURE);
            }
        }
        strBuilder.append(END_FEATURES);
        return new ByteArrayInputStream(strBuilder.toString().getBytes(ENCODING));
    }

    /**
     * Build a Feature from XML TAG.
     * 
     * @param featXmlTag
     *            xml tag to nuild feature
     * @return current feature
     */
    private static Feature parseFeature(Element featXmlTag) {
        NamedNodeMap nnm = featXmlTag.getAttributes();

        if (nnm.getNamedItem(ATT_UID) == null) {
            throw new IllegalArgumentException("Error syntax in configuration file : " + "'uid' is required for each feature");
        }
        String uid = nnm.getNamedItem(ATT_UID).getNodeValue();

        if (nnm.getNamedItem(ATT_ENABLE) == null) {
            throw new IllegalArgumentException("Error syntax in configuration file : "
                    + "'enable' is required for each feature (check " + uid + ")");
        }
        boolean enable = Boolean.valueOf(nnm.getNamedItem(ATT_ENABLE).getNodeValue());

        // Description
        String desc = parserDescription(nnm);

        // Parsing Flip Strategy
        FlippingStrategy flipStrategy = parseFlipStrategy(nnm, uid);

        // Parse authorization
        List<String> authorizations = parseListAuthorizations(featXmlTag);

        return new Feature(uid, enable, desc, authorizations, flipStrategy);
    }

    /**
     * Parser target description.
     * 
     * @param nnm
     *            current working tag
     * @return description of the feature
     */
    private static String parserDescription(NamedNodeMap nnm) {
        String desc = null;
        if (nnm.getNamedItem(ATT_DESC) != null) {
            desc = nnm.getNamedItem(ATT_DESC).getNodeValue();
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
    private static List<String> parseListAuthorizations(Element featXmlTag) {
        List<String> authorizations = null;
        if (featXmlTag.hasChildNodes()) {
            NodeList lisOfAuth = featXmlTag.getElementsByTagName(TAG_AUTH);
            authorizations = new ArrayList<String>();
            for (int idxAuth = 0; idxAuth < lisOfAuth.getLength(); idxAuth++) {
                Element auth = (Element) lisOfAuth.item(idxAuth);
                authorizations.add(auth.getAttributes().getNamedItem(ATT_ROLE).getNodeValue());
            }
        }
        return authorizations;
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
    private static FlippingStrategy parseFlipStrategy(NamedNodeMap nnm, String uid) {
        FlippingStrategy flipStrategy = null;
        if (nnm.getNamedItem(ATT_STRATEGY) != null) {
            try {
                String clazzName = nnm.getNamedItem(ATT_STRATEGY).getNodeValue();
                flipStrategy = (FlippingStrategy) Class.forName(clazzName).newInstance();
                if (nnm.getNamedItem(ATT_EXPRESSION) != null) {
                    String expr = nnm.getNamedItem(ATT_EXPRESSION).getNodeValue();
                    flipStrategy.init(uid, expr);
                }
            } catch (Exception e) {
                throw new IllegalArgumentException("Invalid attribute 'strategy' on feature " + uid, e);
            }
        }
        return flipStrategy;
    }

}
