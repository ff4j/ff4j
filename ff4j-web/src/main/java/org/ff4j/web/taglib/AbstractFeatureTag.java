package org.ff4j.web.taglib;

/*-
 * #%L
 * ff4j-web
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import jakarta.servlet.jsp.JspException;
import jakarta.servlet.jsp.PageContext;
import jakarta.servlet.jsp.tagext.TagSupport;
import org.ff4j.FF4j;
import org.ff4j.core.FlippingExecutionContext;

import java.io.IOException;
import java.util.Map;

import static org.ff4j.web.embedded.ConsoleConstants.FF4J_SESSIONATTRIBUTE_NAME;

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

    /** The variable name where the result will be stored. */
    private String var;

    /** The current scope. */
    private int scope;

    /** FF4j bean name. */
    private final String ff4jAttributeName = FF4J_SESSIONATTRIBUTE_NAME;

    public AbstractFeatureTag() {
        init();
    }

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
        boolean result = false;

        try {
            FF4j ff4j = (FF4j) pageContext.findAttribute(getFf4jAttributeName());

            // Handle where no ff4j available
            if (ff4j == null) {
                displayError("Cannot find FF4J bean as attribute (" + getFf4jAttributeName() + ") in any scope.");
            }

            // Handle where feature doe not exist
            else if (!ff4j.exist(getFeatureid()) && !ff4j.isAutocreate()) {
                displayError("Cannot find feature (" + getFeatureid() + ") anywhere.");
            }

            // Everything is OK
            else if (eval(ff4j, pageContext)) {
                result = true;
            }

        } catch (IOException ioe) {
            throw new JspException("Error occur when processing TAG FF4J", ioe);
        }

        // Expose result to published variable.
        exposeVariables(result);

        return result ? EVAL_BODY_INCLUDE : SKIP_BODY;
    }

    protected boolean eval(FF4j ff4j, PageContext pageContext) {
        FlippingExecutionContext currentContext = ff4j.getCurrentContext();
        FlippingExecutionContext executionContext = isShareHttpSession() ? createExecutionContextFromSharedHttpSession(pageContext, currentContext) : currentContext;

        try {
            return evalWithExecutionContext(ff4j, pageContext, executionContext);
        } finally {
            // Restore original current context, otherwise some parameters from previous JSP context remains.
            if (isShareHttpSession()) {
                ff4j.setCurrentContext(currentContext);
            }
        }
    }

    private FlippingExecutionContext createExecutionContextFromSharedHttpSession(PageContext pageContext, FlippingExecutionContext currentContext) {
        FlippingExecutionContext localExecutionContext = new FlippingExecutionContext(currentContext);
        mergeRequestPageContext(pageContext, localExecutionContext);
        return localExecutionContext;
    }

    private void mergeRequestPageContext(PageContext pageContext, FlippingExecutionContext localExecutionContext) {
        localExecutionContext.putString("LOCALE", pageContext.getRequest().getLocalName());
        mergeRequestParameters(pageContext, localExecutionContext);
    }

    private void mergeRequestParameters(PageContext pageContext, FlippingExecutionContext localExecutionContext) {
        @SuppressWarnings("unchecked")
        Map< String, String[]> parameters = pageContext.getRequest().getParameterMap();

        for (Map.Entry<String,String[]> param : parameters.entrySet()) {
            String[] innerParams = param.getValue();
            if (innerParams != null) {
                StringBuilder sb = new StringBuilder();
                for (String innerParam : innerParams) {
                    sb.append(innerParam);
                    sb.append(",");
                }

                String expression = sb.toString();
                localExecutionContext.putString(param.getKey(), expression.substring(0, expression.length() - 1));
            }
        }
    }

    protected abstract boolean evalWithExecutionContext(FF4j ff4j, PageContext pageContext, FlippingExecutionContext executionContext);

    private void exposeVariables(boolean result) {
        if (var != null) {
            pageContext.setAttribute(var, result, scope);
        }
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

    /**
     * Getter accessor for attribute {@link #var}.
     *
     * @return
     *       current value of {@link #var}
     */
    public String getVar() {
        return var;
    }

    /**
     * Setter accessor for {@link #var}.
     * @param var
     * 		new value for {@link #var}
     */
    public void setVar(String var) {
        this.var = var;
    }

    /**
     * Getter accessor for attribute {@link #scope}.
     *
     * @return
     *       current value of {@link #scope}
     */
    public int getScope() {
        return scope;
    }

    /**
     * Setter accessor for {@link #scope}.
     * @param scope
     * 		new value for {@link #scope}
     */
    public void setScope(int scope) {
        this.scope = scope;
    }

    @Override
    public void release() {
        super.release();
        init();
    }

    private void init() {
        featureid = null;
        shareHttpSession = false;
        var = null;
        scope = PageContext.PAGE_SCOPE;
    }
}
