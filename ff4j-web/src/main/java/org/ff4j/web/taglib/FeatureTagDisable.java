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

import java.util.Map;

import javax.servlet.jsp.PageContext;

import org.ff4j.FF4j;
import org.ff4j.core.FlippingExecutionContext;

/**
 * Content of enclosing tag will be displayed if feature not enable.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureTagDisable extends AbstractFeatureTag {

    /** serial. */
    private static final long serialVersionUID = 2699259876617318061L;
  
    /** {@inheritDoc} */
    protected boolean eval(FF4j ff4j, PageContext jspContext) {
        FlippingExecutionContext executionContext = new FlippingExecutionContext();
        if (isShareHttpSession()) {
            executionContext.putString("LOCALE", pageContext.getRequest().getLocalName());
            @SuppressWarnings("unchecked")
            Map < String, String[]> parameters = pageContext.getRequest().getParameterMap();
            for (Map.Entry<String,String[]> param : parameters.entrySet()) {
                String[] innerParams = param.getValue();
                if (innerParams != null) {
                    StringBuilder sb = new StringBuilder();
                    for (String innerParam : innerParams) {
                        sb.append(innerParam);
                        sb.append(",");
                    }
                    String expression = sb.toString();
                    executionContext.putString(param.getKey(), expression.substring(0, expression.length() - 1));
                }
            }
        }
        return !ff4j.check(getFeatureid(), executionContext);
    }

}