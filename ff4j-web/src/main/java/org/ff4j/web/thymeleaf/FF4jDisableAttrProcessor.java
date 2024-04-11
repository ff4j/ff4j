package org.ff4j.web.thymeleaf;

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

import org.ff4j.FF4j;
import org.thymeleaf.context.ITemplateContext;
import org.thymeleaf.engine.AttributeName;
import org.thymeleaf.model.IProcessableElementTag;

import org.thymeleaf.standard.processor.AbstractStandardConditionalVisibilityTagProcessor;
import org.thymeleaf.templatemode.TemplateMode;

/**
 * Created by benoitmeriaux on 08/01/15.
 */
public class FF4jDisableAttrProcessor extends AbstractStandardConditionalVisibilityTagProcessor {

    protected FF4jDisableAttrProcessor(TemplateMode templateMode, String dialectPrefix, String attrName, int precedence) {
        super(templateMode, dialectPrefix, attrName, precedence);
    }

    @Override
    protected boolean isVisible(ITemplateContext iTemplateContext, IProcessableElementTag iProcessableElementTag, AttributeName attributeName, String s) {
        final String feature = iProcessableElementTag.getAttributeValue(attributeName);
        if (feature == null || feature.trim().equals("")) {
            return false;
        }
        FF4j ff4j = new FF4j();//getFF4j(arguments.getContext());
        return !ff4j.check(feature);
    }
}
