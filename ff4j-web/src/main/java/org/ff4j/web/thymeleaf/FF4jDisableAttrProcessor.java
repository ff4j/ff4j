package org.ff4j.web.thymeleaf;

/*
 * #%L
 * ff4j-sample-web
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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
import org.thymeleaf.Arguments;
import org.thymeleaf.dom.Element;
import org.thymeleaf.processor.attr.AbstractConditionalVisibilityAttrProcessor;

/**
 * Created by benoitmeriaux on 08/01/15.
 */
public class FF4jDisableAttrProcessor extends AbstractConditionalVisibilityAttrProcessor {

    protected FF4jDisableAttrProcessor(final String attributeName) {
        super(attributeName);
    }

    protected FF4jDisableAttrProcessor() {
        super("disable");
    }

    @Override
    public int getPrecedence() {
        // => si feature disabled, c'est ultra prioritaire :-)
        return 10;
    }

    @Override
    protected boolean isVisible(Arguments arguments, Element element, String attributeName) {
        final String feature = element.getAttributeValue(attributeName);
        if (feature == null || feature.trim().equals("")) {
            return false;
        }
        FF4j ff4j = new FF4j();//getFF4j(arguments.getContext());
        return !ff4j.check(feature);
    }

   
}
