package org.ff4j.spring.namespace;

/*
 * #%L
 * ff4j-aop
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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


import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.xml.NamespaceHandlerSupport;

import static org.ff4j.spring.namespace.FF4jNameSpaceConstants.*;

/**
 * Use Spring NameSpace to simplify settings.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4jNameSpaceHandler extends NamespaceHandlerSupport {

    /** Logger statique pour la classe. **/
    private static Log logger = LogFactory.getLog(FF4jNameSpaceHandler.class);

    /**
     * Default Constructor to register Parser in the handler.
     */
    public FF4jNameSpaceHandler() {
        registerBeanDefinitionParser(TAG_FF4J, new FF4jBeanDefinitionParser());
        registerBeanDefinitionParser(TAG_PLACEHOLDER, new FF4JPlaceHolderBeanDefinitionParser());
    }

    /** {@inheritDoc} */
    public void init() {
        logger.debug("Parsing FF4J Spring Namespace Elements");
    }

}
