package org.ff4j.web;

import static org.ff4j.web.embedded.ConsoleConstants.CSS_SESSIONATTRIBUTE_NAME;
import static org.ff4j.web.embedded.ConsoleConstants.FF4J_SESSIONATTRIBUTE_NAME;
import static org.ff4j.web.embedded.ConsoleConstants.SERVLETPARAM_CSS;
import static org.ff4j.web.embedded.ConsoleConstants.SERVLETPARAM_FF4JPROVIDER;

/*
 * #%L
 * ff4j-web
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

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;

import org.ff4j.FF4j;
import org.ff4j.web.console.CustomMessageResolver;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.thymeleaf.TemplateEngine;
import org.thymeleaf.templateresolver.ClassLoaderTemplateResolver;

/**
 * Servlet initialisation to put FF4J in HTTP Session.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4jInitServlet extends HttpServlet {

    /** Serial. */
    private static final long serialVersionUID = 8447941463286918975L;
    
    /** Logger for this class. */
    public static final Logger LOGGER = LoggerFactory.getLogger(FF4jInitServlet.class);
    
    /** instance of ff4j. */
    protected FF4j ff4j = null;

    /** initializing ff4j provider. */
    protected FF4JProvider ff4jProvider = null;
    
    /** Template engine. */
    protected TemplateEngine templateEngine = null;
    
    /**
     * Servlet initialization, init FF4J from "ff4jProvider" attribute Name.
     *
     * @param servletConfig
     *            current {@link ServletConfig} context
     * @throws ServletException
     *             error during servlet initialization
     */
    public void init(ServletConfig servletConfig) throws ServletException {
    	LOGGER.info("  __  __ _  _   _ ");
        LOGGER.info(" / _|/ _| || | (_)");
        LOGGER.info("| |_| |_| || |_| |");
        LOGGER.info("|  _|  _|__   _| |");
        LOGGER.info("|_| |_|    |_|_/ |");
        LOGGER.info("             |__/  v" + getClass().getPackage().getImplementationVersion());
        LOGGER.info(" ");
        
        if (ff4j == null) {
        	initializeFF4J(servletConfig);
        }
        
    	initializeTemplateEngine();
    }
    
    /**
     * Initialize FF4J configuration.
     *
     * @param servletConfig
     * 		current servlet configuration
     */
    private void initializeFF4J(ServletConfig servletConfig) {
    	String className = servletConfig.getInitParameter(SERVLETPARAM_FF4JPROVIDER);
	    try {
	    	Class<?> c = Class.forName(className);
	        Object o = c.newInstance();
	        ff4jProvider = (FF4JProvider) o;
	        LOGGER.info("ff4j context has been successfully initialized - {} feature(s)", ff4jProvider.getFF4j().getFeatures().size());
	    } catch (ClassNotFoundException e) {
	    	throw new IllegalArgumentException("Cannot load ff4jProvider as " + ff4jProvider, e);
	    } catch (InstantiationException e) {
	    	throw new IllegalArgumentException("Cannot instantiate  " + ff4jProvider + " as ff4jProvider", e);
	    } catch (IllegalAccessException e) {
	    	throw new IllegalArgumentException("No public constructor for  " + ff4jProvider + " as ff4jProvider", e);
	    } catch (ClassCastException ce) {
	    	throw new IllegalArgumentException("ff4jProvider expected instance of " + FF4JProvider.class, ce);
	    }
	
	    ff4j = ff4jProvider.getFF4j();
	    servletConfig.getServletContext().setAttribute(FF4J_SESSIONATTRIBUTE_NAME, ff4j);
	    LOGGER.debug("Servlet has been initialized and ff4j store in session with {} ", ff4j.getFeatures().size());
	    String cssFile = servletConfig.getInitParameter(SERVLETPARAM_CSS);
	    if (cssFile != null) {
	    	LOGGER.debug("A custom CSS has been defined [" + cssFile + "]");
	        servletConfig.getServletContext().setAttribute(CSS_SESSIONATTRIBUTE_NAME, cssFile);
	    }
    }
    
    /**
     * Initialize Thymeleaf.
     *
     * @param servletConfig
     */
    private void initializeTemplateEngine() {
    	ClassLoaderTemplateResolver templateResolver = new ClassLoaderTemplateResolver();
	    templateResolver.setTemplateMode("XHTML");
	    templateResolver.setPrefix("views/view-");
	    templateResolver.setSuffix(".dat");
	    templateResolver.setCacheTTLMs(3600000L);
	    
	    templateEngine = new TemplateEngine();
        templateEngine.setTemplateResolver(templateResolver);
        templateEngine.addMessageResolver(new CustomMessageResolver());
        LOGGER.info("Thymeleaf has been initialized");
    }
    
}
