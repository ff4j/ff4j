package org.ff4j.spring.cloudconfig.source;

/*
 * #%L
 * ff4j-store-spring-cloudconfig
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Custom bootstrap configuration to fetch properties from FF4J as Well.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
@Configuration
public class FF4jBootstrapConfiguration {

    @Configuration
    @EnableConfigurationProperties
    @ConditionalOnProperty(name = "spring.cloud.custom.config.enabled", matchIfMissing = true)
    protected static class FF4jPropertySourceConfiguration {
        
        /** Inject at startup. */
        private FF4j ff4j = null;
        
        /**
         * Inject through constructor.
         *
         * @param ff4j
         *          current ff4j bean
         */
        public FF4jPropertySourceConfiguration(FF4j ff4j) {
            this.ff4j = ff4j;
            System.out.println(ff4j);
        }
        
        /**
         * Expected 
         * @return
         */
        @Bean
        public FF4jPropertySourceLocator customPropertySourceLocator() {
            return new FF4jPropertySourceLocator(ff4j);
        }
    }
}

