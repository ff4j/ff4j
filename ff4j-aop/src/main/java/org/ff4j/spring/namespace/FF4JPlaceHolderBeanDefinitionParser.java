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
import org.ff4j.spring.placeholder.FF4jPropertiesPlaceHolderConfigurer;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanDefinitionHolder;
import org.springframework.beans.factory.config.RuntimeBeanReference;
import org.springframework.beans.factory.support.AbstractBeanDefinition;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.BeanDefinitionReaderUtils;
import org.springframework.beans.factory.xml.BeanDefinitionParser;
import org.springframework.beans.factory.xml.ParserContext;
import org.w3c.dom.Element;

import static org.ff4j.spring.namespace.FF4jNameSpaceConstants.*;

/**
 * Custom and simple implementation of a {@link BeanDefinitionParser} to create and {@link FF4jPropertiesPlaceHolderConfigurer}.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4JPlaceHolderBeanDefinitionParser implements BeanDefinitionParser {

    /** logger for class. */
    private static Log logger = LogFactory.getLog(FF4jBeanDefinitionParser.class);
    
    /** {@inheritDoc} */
    public final BeanDefinition parse(Element element, ParserContext parserContext) {
        logger.debug("Initialization from <ff4j:" + TAG_PLACEHOLDER + "> TAG");
        BeanDefinitionBuilder builder = BeanDefinitionBuilder.genericBeanDefinition();
        builder.getRawBeanDefinition().setBeanClass(FF4jPropertiesPlaceHolderConfigurer.class);
        builder.getRawBeanDefinition().setSource(parserContext.extractSource(element));

        // Reference to FF4J bean
        RuntimeBeanReference refFF4j = new RuntimeBeanReference("ff4j");
        builder.getBeanDefinition().getPropertyValues().addPropertyValue("id", BEANID_PLACEHOLDER_CONF);
        builder.getBeanDefinition().getPropertyValues().addPropertyValue("ff4j", refFF4j);
        builder.getBeanDefinition().getPropertyValues().addPropertyValue("order", 2);
        
        AbstractBeanDefinition def = builder.getBeanDefinition();
        if (def != null && !parserContext.isNested()) {
            BeanDefinitionHolder holder = new BeanDefinitionHolder(def, BEANID_PLACEHOLDER,  new String[0]);
            BeanDefinitionReaderUtils.registerBeanDefinition(holder, parserContext.getRegistry());
        }
        return def;
    }
  
}
