package org.ff4j.web.api;

import io.swagger.config.ScannerFactory;
import io.swagger.jaxrs.config.BeanConfig;

/**
 * Swagger configuration is shared between different JAXRS implementations.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FF4jSwaggerConfiguration {
    
    private static BeanConfig beanConfig;
    
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
