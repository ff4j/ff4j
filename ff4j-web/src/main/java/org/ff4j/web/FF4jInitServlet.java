package org.ff4j.web;

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

import org.ff4j.web.embedded.ConsoleConstants;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Servlet initialisation to put FF4J in HTTP Session.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4jInitServlet extends HttpServlet implements ConsoleConstants {

    /** Serial. */
    private static final long serialVersionUID = 8447941463286918975L;
    
    /** Logger for this class. */
    public Logger logger = LoggerFactory.getLogger(getClass());

    /** {@inheritDoc} */
    @Override
    public void init(ServletConfig servletConfig) throws ServletException {
        logger.debug("Insert FF4J into web session");
        String className = servletConfig.getInitParameter(PROVIDER_PARAM_NAME);
        if (className == null) {
            throw new IllegalStateException("Cannot initialize Servlet " 
                        + getClass() + " : expecting parameter'" 
                        + PROVIDER_PARAM_NAME + "'");
        }
        
        FF4JProvider ff4jProvider = null;
        try {
            Class<?> c = Class.forName(className);
            Object o = c.newInstance();
            ff4jProvider = (FF4JProvider) o;
            logger.info("  __  __ _  _   _ ");
            logger.info(" / _|/ _| || | (_)");
            logger.info("| |_| |_| || |_| |");
            logger.info("|  _|  _|__   _| |");
            logger.info("|_| |_|    |_|_/ |");
            logger.info("             |__/  .v" + getClass().getPackage().getImplementationVersion());
            logger.info(" ");
            
            servletConfig.getServletContext().setAttribute(FF4J_SESSIONATTRIBUTE_NAME, ff4jProvider.getFF4j());
            logger.info("ff4j class is now available in Session under 'FF4J' attribute name (required for taglib)");
            
        } catch (ClassNotFoundException e) {
            throw new IllegalArgumentException("Cannot load ff4jProvider as " + ff4jProvider, e);
        } catch (InstantiationException e) {
            throw new IllegalArgumentException("Cannot instantiate  " + ff4jProvider + " as ff4jProvider", e);
        } catch (IllegalAccessException e) {
            throw new IllegalArgumentException("No public constructor for  " + ff4jProvider + " as ff4jProvider", e);
        } catch (ClassCastException ce) {
            throw new IllegalArgumentException("ff4jProvider expected instance of " + FF4JProvider.class, ce);
        }
    }
    
}
