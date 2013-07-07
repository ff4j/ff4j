package org.ff4j.web;

import java.io.IOException;

import javax.servlet.jsp.JspException;
import javax.servlet.jsp.JspWriter;
import javax.servlet.jsp.tagext.BodyContent;
import javax.servlet.jsp.tagext.BodyTagSupport;

import org.ff4j.Feature;
import org.ff4j.FF4j;
import org.ff4j.store.FeatureStore;


/**
 * Taglib to filter display based on {@link Feature} status within {@link FeatureStore}.
 *
 * <p>Sample use
 * <p><pre>
 * 	&lt;ff4j:enable featureid="mercure-desc"&gt;
 * 		here your html code
 *  &lt;/ff4j:enable@gt;
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FlipTag extends BodyTagSupport {

	/** serial. */
	private static final long serialVersionUID = -4924423673988080781L;
	
	/** Injected by JSP itSelf. */
	private String featureid = "";
	
	/** {@inheritDoc} */
	public int doAfterBody() throws JspException {
		try {
			// Checking Feature value
			if (FF4j.isFlipped(getFeatureid())) {
				BodyContent bodycontent = getBodyContent();
				String body = bodycontent.getString();
				JspWriter out = bodycontent.getEnclosingWriter();
				if (body != null) {
					out.print(body);
				}
			}
		} catch (IOException ioe) {
			throw new JspException("Error occur when processing TAG FF4J", ioe);
		}
		return SKIP_BODY;
	}

	/**
	 * Getter accessor for attribute 'featureid'.
	 *
	 * @return
	 *       current value of 'featureid'
	 */
	public String getFeatureid() {
		return featureid;
	}

	/**
	 * Setter accessor for attribute 'featureid'.
	 * @param featureid
	 * 		new value for 'featureid '
	 */
	public void setFeatureid(String featureid) {
		this.featureid = featureid;
	}

}
