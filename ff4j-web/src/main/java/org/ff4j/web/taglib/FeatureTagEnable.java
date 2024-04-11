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


import jakarta.servlet.jsp.PageContext;
import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlippingExecutionContext;

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
 * <p/>
 *
 * It is also possible to store the result of feature evaluation to be able to use it in a more complex
 * condition:
 *
 * <pre><code>
 *     &lt;ff4j:disable featureid="mercure-desc" var="mercureDescEnabled"&gt;
 *     &lt;c:if test=${mercureDescEnabled && otherCondition}"&gt;
 *       Your HTML code
 *     &lt;/c:if"&gt;
 * </code></pre>

 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureTagEnable extends AbstractFeatureTag {

    /** serial number. */
    private static final long serialVersionUID = -4924423673988080781L;

    /** {@inheritDoc} */
    protected boolean evalWithExecutionContext(FF4j ff4j, PageContext pageContext, FlippingExecutionContext executionContext) {
        return ff4j.check(getFeatureid(), executionContext);
    }
}
