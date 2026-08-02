package org.ff4j.aop.cglib;

/*-
 * #%L
 * ff4j-aop
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

import org.ff4j.aop.FeatureAutoProxy;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

/**
 * Forces {@link FeatureAutoProxy} to use CGLIB (subclass) proxies, reproducing the
 * Spring Boot 4 default behaviour where {@code spring.aop.proxy-target-class=true}.
 */
@Configuration
@Profile("proxyTargetClass")
public class ProxyTargetClassConfiguration {

    @Bean
    public static BeanFactoryPostProcessor forceFeatureAutoProxyCglib() {
        return new BeanFactoryPostProcessor() {
            @Override
            public void postProcessBeanFactory(ConfigurableListableBeanFactory beanFactory) throws BeansException {
                beanFactory.getBeanDefinition("ff.autoproxy")
                           .getPropertyValues()
                           .add("proxyTargetClass", true);
            }
        };
    }
}
