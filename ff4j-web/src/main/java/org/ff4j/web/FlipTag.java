package org.ff4j.web;

/*
 * #%L
 * FlipTag.java (ff4j-web) by Cedrick LUNVEN
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

import java.io.IOException;

import javax.servlet.jsp.JspException;
import javax.servlet.jsp.JspWriter;
import javax.servlet.jsp.tagext.BodyContent;
import javax.servlet.jsp.tagext.BodyTagSupport;

import org.ff4j.FF4j;
import org.ff4j.exception.FeatureNotFoundException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Taglib to filter display based on {@link Feature} status within {@link FeatureStore}.
 * 
 * <p>
 * Sample use
 * <p>
 * 
 * <pre>
 * &lt;ff4j:enable featureid="mercure-desc"&gt;
 * here your html code
 *  &lt;/ff4j:enable@gt;
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FlipTag extends BodyTagSupport {

    /** serial number. */
    private static final long serialVersionUID = -4924423673988080781L;

    /** Logger for Advisor. */
    static final Logger LOGGER = LoggerFactory.getLogger(FlipTag.class);

    /** Injected by JSP itSelf. */
    private String featureid = "";

    private static FF4j ff4j;

    /** {@inheritDoc} */
    @Override
    public int doAfterBody() throws JspException {
        try {
            // Checking Feature value
            if (getFf4j().isFlipped(getFeatureid())) {
                BodyContent bodycontent = getBodyContent();
                String body = bodycontent.getString();
                JspWriter out = bodycontent.getEnclosingWriter();
                if (body != null) {
                    out.print(body);
                }
            }
        } catch (FeatureNotFoundException fef) {
            LOGGER.warn("This feature foes not exis");
        } catch (IOException ioe) {
            throw new JspException("Error occur when processing TAG FF4J", ioe);
        }
        return SKIP_BODY;
    }

    /**
     * Getter accessor for attribute 'featureid'.
     * 
     * @return current value of 'featureid'
     */
    public String getFeatureid() {
        return featureid;
    }

    /**
     * Setter accessor for attribute 'featureid'.
     * 
     * @param featureid
     *            new value for 'featureid '
     */
    public void setFeatureid(String featureid) {
        this.featureid = featureid;
    }

    /**
     * Getter accessor for attribute 'ff4j'.
     * 
     * @return current value of 'ff4j'
     */
    public static FF4j getFf4j() {
        return ff4j;
    }

    /**
     * Setter accessor for attribute 'ff4j'.
     * 
     * @param ff4j
     *            new value for 'ff4j '
     */
    public static void setFf4j(FF4j ff4j) {
        FlipTag.ff4j = ff4j;
    }

}
