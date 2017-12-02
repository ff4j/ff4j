package org.ff4j.spring.boot.web.api.config;

import com.google.common.collect.Lists;
import org.ff4j.FF4j;

/*
 * #%L
 * ff4j-spring-boot-web-api
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

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import springfox.documentation.builders.PathSelectors;
import springfox.documentation.builders.RequestHandlerSelectors;
import springfox.documentation.service.ApiInfo;
import springfox.documentation.service.Contact;
import springfox.documentation.spi.DocumentationType;
import springfox.documentation.spring.web.plugins.Docket;
import springfox.documentation.swagger2.annotations.EnableSwagger2;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
@Configuration
@EnableSwagger2
public class SwaggerConfig {

    @Bean
    public Docket api() {
        return new Docket(DocumentationType.SWAGGER_2)
            .groupName("FF4j")
            .select()
            .apis(RequestHandlerSelectors.basePackage("org.ff4j.spring.boot.web.api.resources"))
            .paths(PathSelectors.any())
            .build().apiInfo(apiInfo())
            .useDefaultResponseMessages(false);
    }

    /**
     * Initialization of documentation
     *
     * @return static infos
     */
    private ApiInfo apiInfo() {
        return new ApiInfo(
            "ff4j (ff4j.org) Feature Toggle for Java Platform",
            "Operation FF4J solution through API (features, properties, audit)",
            FF4j.class.getPackage().getImplementationVersion(),
            "Terms of service",
            new Contact("Paul William", "", "paul589140480@gmail.com"),
            "Apache 2.0", "http://www.apache.org/licenses/LICENSE-2.0.html",
            Lists.newArrayList());
    }
}
