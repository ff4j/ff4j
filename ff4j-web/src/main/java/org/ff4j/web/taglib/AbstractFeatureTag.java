package org.ff4j.web.taglib;

/*
 * #%L
 * ff4j-web
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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
import javax.servlet.jsp.PageContext;
import javax.servlet.jsp.tagext.TagSupport;

import org.ff4j.FF4j;

import static org.ff4j.web.embedded.ConsoleConstants.*;

/**
 * Parent class for FF4J TagLib library.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public abstract class AbstractFeatureTag extends TagSupport {

    /** serial number. */
    private static final long serialVersionUID = 3967425494402133171L;

    /** Error message constant. */
    protected static final String ERROR_MSG_START = "<p><span style=\"color:red;font-weight:bold\">ERROR &lt;ff4j:*&gt; :";

    /** Error message constant. */
    protected static final String ERROR_MSG_END = "</span>";

    /** Injected by JSP itSelf. */
    private String featureid = "";
    
    /** Share httpSession with Flipping execution context, as consuming keep at minimum. */
    private boolean shareHttpSession = false;

    /** FF4j bean name. */
    private final String ff4jAttributeName = FF4J_SESSIONATTRIBUTE_NAME;

    /**
     * Display an error message in tag.
     *
     * @param message
     *            target message
     * @throws IOException
     *             error
     */
    protected void displayError(String message) throws IOException {
        pageContext.getOut().print(ERROR_MSG_START + " " + message + ERROR_MSG_END);
    }
    
    /** {@inheritDoc} */
    @Override
    public int doStartTag() throws JspException {
        try {

            FF4j ff4j = (FF4j) pageContext.findAttribute(getFf4jAttributeName());

            // Handle where no ff4j available
            if (ff4j == null) {
                displayError("Cannot find FF4J bean as attribute (" + getFf4jAttributeName() + ") in any scope.");
                return SKIP_BODY;
            }

            // Handle where feature doe not exist
            if (!ff4j.exist(getFeatureid()) && !ff4j.isAutocreate()) {
                displayError("Cannot find feature (" + getFeatureid() + ") anywhere.");
                return SKIP_BODY;
            }

            // Everything is OK
            if (eval(ff4j, pageContext)) {
                return EVAL_BODY_INCLUDE;
            }

        } catch (IOException ioe) {
            throw new JspException("Error occur when processing TAG FF4J", ioe);
        }
        return SKIP_BODY;
    }
    
    protected abstract boolean eval(FF4j ff4j, PageContext pageContext);

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
     * Getter accessor for attribute 'ff4jAttributeName'.
     * 
     * @return current value of 'ff4jAttributeName'
     */
    public String getFf4jAttributeName() {
        return ff4jAttributeName;
    }

    /**
     * Getter accessor for attribute 'shareHttpSession'.
     *
     * @return
     *       current value of 'shareHttpSession'
     */
    public boolean isShareHttpSession() {
        return shareHttpSession;
    }

    /**
     * Setter accessor for attribute 'shareHttpSession'.
     * @param shareHttpSession
     * 		new value for 'shareHttpSession '
     */
    public void setShareHttpSession(boolean shareHttpSession) {
        this.shareHttpSession = shareHttpSession;
    }

}
