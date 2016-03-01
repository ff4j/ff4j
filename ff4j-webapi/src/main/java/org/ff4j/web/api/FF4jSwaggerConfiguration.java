package org.ff4j.web.api;

/*
 * #%L
 * ff4j-webapi
 * %%
 * Copyright (C) 2013 - 2015 FF4J
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


import io.swagger.config.ScannerFactory;
import io.swagger.jaxrs.config.BeanConfig;

/**
 * Swagger configuration is shared between different JAXRS implementations.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public final class FF4jSwaggerConfiguration {
    
    private static BeanConfig beanConfig;
    
    private FF4jSwaggerConfiguration() {}
    
    static {
        beanConfig = new BeanConfig();
        beanConfig.setTitle("FF4J (ff4j.org) WebAPI");
        beanConfig.setDescription("Administrate and operate all tasks on your features through this api");
        beanConfig.setResourcePackage("org.ff4j.web.api.resources");
        beanConfig.setContact("@clunven");
        beanConfig.setLicense("Apache 2.0");
        beanConfig.setLicenseUrl("http://www.apache.org/licenses/LICENSE-2.0.html");
        beanConfig.setScan(true);
        ScannerFactory.setScanner(beanConfig);
    }
    /**
     * Allows to TODO
     * @return
     */
    public static BeanConfig getBeanConfig() {
        return beanConfig;
    }

}
