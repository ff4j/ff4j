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

import jakarta.servlet.jsp.tagext.Tag;
import org.ff4j.FF4j;
import org.ff4j.core.FlippingExecutionContext;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mockito;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockPageContext;

import static org.ff4j.web.embedded.ConsoleConstants.FF4J_SESSIONATTRIBUTE_NAME;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;

public class FeatureTagEnableTest {

    private FF4j ff4j;
    private FeatureTagEnable featureTagEnable;

    @Before
    public void setUp() {
        ff4j = spy(new FF4j());
        ff4j.createFeature("my-awesome-feature");
        ff4j.enable("my-awesome-feature");

        MockPageContext pageContext = new MockPageContext();
        pageContext.setAttribute(FF4J_SESSIONATTRIBUTE_NAME, ff4j);
        ((MockHttpServletRequest) pageContext.getRequest()).setLocalName("localhost");

        featureTagEnable = new FeatureTagEnable();
        featureTagEnable.setPageContext(pageContext);
        featureTagEnable.setFeatureid("my-awesome-feature");
        featureTagEnable.setShareHttpSession(false);
    }

    @Test
    public void it_should_render_feature_using_current_execution_context() throws Exception {
        FlippingExecutionContext executionContext = ff4j.getCurrentContext();

        int result = featureTagEnable.doStartTag();

        Assert.assertEquals(Tag.EVAL_BODY_INCLUDE, result);
        Assert.assertSame(executionContext, ff4j.getCurrentContext());
        Mockito.verify(ff4j).check("my-awesome-feature", executionContext);
    }

    @Test
    public void it_should_render_feature_using_new_execution_context_because_of_http_session_sharing() throws Exception {
        FlippingExecutionContext executionContext = ff4j.getCurrentContext();

        featureTagEnable.setShareHttpSession(true);
        int result = featureTagEnable.doStartTag();

        Assert.assertEquals(Tag.EVAL_BODY_INCLUDE, result);
        Assert.assertSame(executionContext, ff4j.getCurrentContext());
        Mockito.verify(ff4j, never()).check("my-awesome-feature", executionContext);

        ArgumentCaptor<FlippingExecutionContext> argContext = ArgumentCaptor.forClass(FlippingExecutionContext.class);
        Mockito.verify(ff4j).check(Mockito.eq("my-awesome-feature"), argContext.capture());

        FlippingExecutionContext localExecutionContext = argContext.getValue();
        Assert.assertFalse(localExecutionContext.isEmpty());
        Assert.assertEquals("localhost", localExecutionContext.getString("LOCALE"));
    }
}
