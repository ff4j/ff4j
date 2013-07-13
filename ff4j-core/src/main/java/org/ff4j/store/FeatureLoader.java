package org.ff4j.store;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.ff4j.Feature;
import org.ff4j.strategy.FlippingStrategy;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.NodeList;


/**
 * Import Features from a well formated XML file.
 * 
 * <p>Sample File : 
 * <p><pre>
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
public class FeatureLoader {

	/** XML Tags and attribute. */
	public static final String TAG_FEATURES  = "features";
	
	/** XML Tags and attribute. */
	public static final String TAG_FEATURE   = "feature";
	
	/** XML Tags and attribute. */
	public static final String TAG_AUTH  	 = "auth";
	
	/** XML Tags and attribute. */
	public static final String ATT_UID 		 = "uid";
	
	/** XML Tags and attribute. */
	public static final String ATT_DESC 	 = "description";
	
	/** XML Tags and attribute. */
	public static final String ATT_ENABLE 	 = "enable";
	
	/** XML Tags and attribute. */
	public static final String ATT_ROLE 	 = "role";
	
	public static final String ATT_STRATEGY	 = "strategy";
	
	public static final String ATT_EXPRESSION = "expression";
	
	/** XML Generation constants. */
	private static final String ENCODING	 = "UTF-8";
	
	/** XML Generation constants. */
	private static final String XML_HEADER   = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n<features>\n\n";
	
	/** XML Generation constants. */
	private static final String XML_FEATURE  = " <feature uid=\"{0}\" description=\"{1}\" enabled=\"{2}\">\n";
	
	/** XML Generation constants. */
	private static final String XML_AUTH	 = "   <auth role=\"{0}\" />\n";
	
	/** XML Generation constants. */
	private static final String END_FEATURE  = " </feature>\n\n";
	
	/** XML Generation constants. */
	private static final String END_FEATURES = "</features>";
	
	/**
	 * Load map of {@link Feature} from an inpustream (containing xml text).
	 *
	 * @param in
	 * 		inpustream with XML text
	 * @return
	 * 		the sorted map of features
	 * @throws IOException
	 * 		exception raised when reading inputstream
	 */
	public static LinkedHashMap<String, Feature> loadFeatures(InputStream in) {
		if (in == null) {
			throw new IllegalArgumentException("Cannot read features file, check path and access rights");
		}
		LinkedHashMap < String, Feature > xmlFeatures = new LinkedHashMap<String, Feature>();
		try {
			// DOM Parsing (small files, simple syntax)
			DocumentBuilder builder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
			Document featuresTag = builder.parse(in);
			if (featuresTag.hasChildNodes()) {
				NodeList listOfFeature = featuresTag.getElementsByTagName(TAG_FEATURE);
				for(int i=0; i<listOfFeature.getLength(); i++) {
					Element      feat	 = (Element) listOfFeature.item(i);
					NamedNodeMap nnm 	 = feat.getAttributes();
					if (nnm.getNamedItem(ATT_UID) == null) {
						throw new IllegalArgumentException("Error syntax in configuration file : " +
								"'uid' is required for each feature");
					}
					String uid = nnm.getNamedItem(ATT_UID).getNodeValue();
					if (nnm.getNamedItem(ATT_ENABLE) == null) {
						throw new IllegalArgumentException("Error syntax in configuration file : " +
								"'enable' is required for each feature (check " + uid + ")");
					}
					
					
					boolean		 enable	 = Boolean.valueOf(nnm.getNamedItem(ATT_ENABLE).getNodeValue());
					// Description
					String desc = null;
					if (nnm.getNamedItem(ATT_DESC) != null) {
						desc = nnm.getNamedItem(ATT_DESC).getNodeValue();
					}
					// Parsing Flip Strategy
					FlippingStrategy flipStrategy = null;
					if (nnm.getNamedItem(ATT_STRATEGY) != null) {
						try {
							String clazzName = nnm.getNamedItem(ATT_STRATEGY).getNodeValue();
							flipStrategy = (FlippingStrategy) Class.forName(clazzName).newInstance();
							if (nnm.getNamedItem(ATT_EXPRESSION) != null) {
								String expr = nnm.getNamedItem(ATT_EXPRESSION).getNodeValue();
								flipStrategy.init(uid, expr);
							}
						} catch(Exception e) {
							throw new IllegalArgumentException("Invalid attribute 'strategy' on feature " + uid, e);
						}
					}
					// Parse authorization
					List < String > authorizations = null;
					if (feat.hasChildNodes()) {
						NodeList lisOfAuth = feat.getElementsByTagName(TAG_AUTH);
						authorizations = new ArrayList<String>();
						for(int idxAuth=0; idxAuth<lisOfAuth.getLength(); idxAuth++){
							Element auth = (Element) lisOfAuth.item(idxAuth);
							authorizations.add(auth.getAttributes().getNamedItem(ATT_ROLE).getNodeValue());
						}
					}
					xmlFeatures.put(uid, new Feature(uid, enable, desc, authorizations, flipStrategy));
				}
			}
		} catch (Exception e) {
			throw new IllegalArgumentException("Cannot parse XML data, please check source file ", e);
		}
		return xmlFeatures;
	}
	
	
	/**
	 * Create XML output stream from a map of {@link Feature}.
	 *
	 * @param f
	 * 		map of features
	 * @return
	 * 		streams
	 * @throws IOException
	 * 		error occurs when generating output
	 */
	public static InputStream exportFeatures(LinkedHashMap<String, Feature> mapOfFeatures)
	throws IOException {
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

}
